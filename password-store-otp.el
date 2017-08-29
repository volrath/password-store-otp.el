;;; password-store-otp.el --- 
;; 
;; Filename: password-store-otp.el
;; Description: 
;; Author: Daniel Barreto
;; Created: Tue Aug 22 13:46:01 2017 (+0200)
;; Version: 0.1.0
;; Package-Requires: ()
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Doc URL: 
;; Keywords: tools
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; This package provides functions for working with the pass-otp
;; extension for pass ("the standard Unix password manager").
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'password-store)
(require 'seq)
(require 's)

(defcustom password-store-otp-screenshots-path nil
  "OTP screenshots directory."
  :group 'password-store
  :type 'string)

(defun password-store-otp--otpauth-lines (lines)
  (seq-filter (lambda (l) (string-prefix-p "otpauth://" l))
              lines))

(defun password-store-otp--get-uri (entry)
  "Own version that produces error if ENTRY has no otp uri."
  (setq url (car (password-store-otp--otpauth-lines
                  (s-lines (password-store--run-show entry)))))
  (when (not url)
    (error "No OTP url found"))
  url)

(defun password-store-otp--safe-copy (secret)
  "Add SECRET to kill ring.

Clear previous password from kill ring.  Pointer to kill ring is
stored in `password-store-kill-ring-pointer'.  SECRET is cleared
after `password-store-timeout' seconds."
  (password-store-clear)
  (kill-new secret)
  (setq password-store-kill-ring-pointer kill-ring-yank-pointer)
  (setq password-store-timeout-timer
        (run-at-time (password-store-timeout) nil 'password-store-clear)))

(defun password-store-otp--insert (entry secret &optional append)
  (message "%s" (shell-command-to-string (format "echo %s | %s otp %s -f %s"
                                                 (shell-quote-argument secret)
                                                 password-store-executable
                                                 (if append "append" "insert")
                                                 (shell-quote-argument entry)))))

(defun password-store-otp--get-qr-image-filename (entry)
  "Return a qr-image-filename for given ENTRY."
  (let ((entry-base (file-name-nondirectory entry)))
    (if password-store-otp-screenshots-path
        (let ((fname (format "%s-%s.png"
                             entry-base
                             (format-time-string "%Y-%m-%dT%T"))))
          (concat (file-name-as-directory password-store-otp-screenshots-path)
                  fname))
      (format "/tmp/%s.png" (make-temp-name entry-base)))))

(defun password-store-otp-code (entry)
  (password-store--run "otp" entry))

(defun password-store-otp-uri (entry)
  (password-store--run "otp" "uri" entry))


;;; Interactive functions

(defun password-store-otp-code-copy (entry)
  (interactive)
  (password-store-otp--safe-copy (password-store-otp-code entry))
  (message "Copied %s to the kill ring. Will clear in %s seconds." entry (password-store-timeout)))

(defun password-store-otp-uri-copy (entry)
  (interactive)
  (password-store-otp--safe-copy (password-store-otp-uri entry))
  (message "Copied %s to the kill ring. Will clear in %s seconds." entry (password-store-timeout)))

(defun password-store-otp-qrcode (entry &optional type)
  (interactive (list (read-string "Password entry: ")))
  (if type
      (shell-command-to-string (format "qrcode -o - -t%s %s"
                                       type
                                       (shell-quote-argument (password-store-otp--get-uri entry))))
    (password-store--run "otp" "uri" "-q" entry)))

(defun password-store-otp-insert (entry otp-uri)
  "Insert a new ENTRY containing OTP-URI."
  (interactive (list (read-string "Password entry: ")
                     (read-passwd "OTP URI: " t)))
  (password-store-otp--insert entry otp-uri))

(defun password-store-otp-append (entry otp-uri)
  "Append to an ENTRY the given OTP-URI."
  (interactive (list (read-string "Password entry: ")
                     (read-passwd "OTP URI: " t)))
  (password-store-otp--insert entry otp-uri t))

(defun password-store-otp-append-from-image (entry)
  "Check clipboard for an image and scan it to get a value otp uri, append it into ENTRY."
  (interactive (list (read-string "Password entry: ")))
  (let ((qr-image-filename (password-store-otp--get-qr-image-filename entry)))
    (when (not (zerop (call-process "import" nil nil nil qr-image-filename)))
      (error "Couldn't get image from clipboard"))
    (password-store-otp-append
     entry
     (shell-command-to-string (format "zbarimg -q --raw %s"
                                      (shell-quote-argument qr-image-filename))))
    (when (not password-store-otp-screenshots-path)
      (delete-file qr-image-filename))))

(provide 'password-store-otp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; password-store-otp.el ends here
