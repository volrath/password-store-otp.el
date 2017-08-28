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
(require 's)

(defgroup password-store-otp '()
  "Emacs mode for password-store-otp."
  :prefix "password-store-otp-"
  :group 'password-store)

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

(defun password-store-otp--otpauth-lines (lines)
  (seq-filter (lambda (l) (string-prefix-p "otpauth://" l))
              lines))

(defun password-store-otp--get-uri (entry)
  "Own version that produces error if entry has no otp uri"
  (if-let ((url (car (password-store-otp--otpauth-lines
                      (s-lines (password-store--run-show entry))))))
      url
    (error "No OTP url found.")))

(defun password-store-otp--insert (entry secret &optional append)
  (message "%s" (shell-command-to-string (format "echo %s | %s otp %s -f %s"
                                                 (shell-quote-argument secret)
                                                 password-store-executable
                                                 (if append "append" "insert")
                                                 (shell-quote-argument entry)))))

;;; Interactive functions

(defun password-store-opt-get (entry)
  (password-store--run "otp" entry))

(defun password-store-otp-uri (entry)
  (password-store--run "otp" "uri" entry))

(defun password-store-otp-copy (entry)
  (interactive)
  (password-store-otp--safe-copy (password-store-otp-get entry))
  (message "Copied %s to the kill ring. Will clear in %s seconds." entry (password-store-timeout)))

(defun password-store-otp-uri-copy (entry)
  (interactive)
  (password-store-otp--safe-copy (password-store-otp-uri entry))
  (message "Copied %s to the kill ring. Will clear in %s seconds." entry (password-store-timeout)))

(defun password-store-otp-qrcode (entry &optional type)
  (interactive (list (read-string "Password entry: ")))
  (if type
      (password-store--run "otp" "uri" "-q" (format "-t%s" type) )
    (password-store--run "otp" "uri" "-q" entry)))

(defun password-store-otp-insert (entry otp-uri)
  "Insert a new ENTRY containing OTP-URI."
  (interactive (list (read-string "Password entry: ")
                     (read-passwd "OTP URI: " t)))
  (password-store-otp--insert entry otp-uri))

(defun password-store-otp-append (entry otp-uri)
  "Insert a new ENTRY containing OTP-URI."
  (interactive (list (read-string "Password entry: ")
                     (read-passwd "OTP URI: " t)))
  (password-store-otp--insert entry otp-uri t))

(provide 'password-store-otp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; password-store-otp.el ends here
