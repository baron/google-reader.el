;;; google-reader.el ---
;;
;; Filename: google-reader.el
;; Description: A mode for google reader
;; Author: Sam Baron
;; Maintainer: Sam Baron
;; Created: Tue Nov 23 12:44:54 2010 (+0900)
;; Version: 0.1
;; Last-Updated:
;;           By:
;;     Update #: 0
;; URL: https://github.com/baron/google-reader.el
;; Keywords: Google Reader, RSS, News
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'cl)
(require 'json)

(defcustom google-reader-account nil "your google account name (use full form ie XXXXX@gmail.com)" :group 'google-reader)
(defcustom google-reader-password nil "your google password name" :group 'google-reader)

(defvar google-reader-client-login-url "https://www.google.com/accounts/ClientLogin"
  "base url for Google authentication")

(defvar google-reader-authentication-header-format "accountType=HOSTED_OR_GOOGLE&Email=%s&Passwd=%s&service=reader"
  "format for auth credential header for Google authentication")

(defvar google-reader-auth-token-header-format "Authorization: GoogleLogin auth=%s"
  "header format for authenticated requests")

;; once users are able to specify url retrieval program the variables
;; below should be converted to defcustom

(defvar google-reader-use-url-retrieve nil
  "If nil, use external command-line HTTP client instead.")

(defvar google-reader-url-retrieval-program "curl"
  "NOT IMPLEMENTED: URL retrieving program used when `install-elisp-use-url-retrieve' is nil.
  you may want to try or the native program \"wget -q -O- %s\"")

(defvar google-reader-auth-token-buffer-name "*google-reader auth token*"
  "this buffer is used for grabbing auth token")

(defvar google-reader-auth-token-string nil
  "this string will be used for all authentication requests")

;;  These are various urls needed to access the Google Reader api
(defvar google-reader-reading-list-url-format "http://www.google.com/reader/api/0/stream/contents/user/-/state/com.google/reading-list?xt=user/-/state/com.google/read&n=%d%s&client=scroll"
  "This url fetches unread items where %d is the number of items per fetch and %s is where the continuation string will go if any")

(defun google-reader-kill-token-buffer ()
  (if (get-buffer google-reader-auth-token-buffer-name)
      (kill-buffer google-reader-auth-token-buffer-name)))

(defun google-reader-authenticate ()
  (google-reader-kill-token-buffer)
  (let* ((gr-header (format google-reader-authentication-header-format google-reader-account google-reader-password)))
    ;;; this calls an asynchronous process ("curl, etc.") that writes output to temp buffer
    (start-process "google-reader-account-auth"
                   google-reader-auth-token-buffer-name
                   google-reader-url-retrieval-program
                   "--silent"
                   "-d"
                   gr-header
                   google-reader-client-login-url)))

(defun google-reader-set-auth-token ()
  (google-reader-authenticate)
  (set-buffer (get-buffer google-reader-auth-token-buffer-name))
  (goto-char (point-min))
  (re-search-forward "Auth=\\([a-zA-Z0-9-_]+\\)" nil t nil)
  (setq google-reader-auth-token-string (match-string 1))
  (message google-reader-auth-token-string)
  (google-reader-kill-token-buffer))

(defun google-reader-get-url (url)
  (let* ((gr-header (format google-reader-auth-token-header-format google-reader-auth-token-string)))
    (start-process "google-reader-get-url"
                   "temp buffer name"
                   google-reader-url-retrieval-program
                   "--silent"
                   "-d"
                   gr-header
                   url)))


;; the declarations below are for testing purposes
;; (google-reader-set-auth-token)


(provide 'google-reader)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; google-reader.el ends here
