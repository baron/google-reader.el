;;; greader.el ---
;;
;; Filename: greader.el
;; Description: A mode for google reader
;; Author: Sam Baron
;; Maintainer: Sam Baron
;; Created: Tue Nov 23 12:44:54 2010 (+0900)
;; Version: 0.1
;; Last-Updated:
;;           By:
;;     Update #: 0
;; URL: https://github.com/baron/greader
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

(defcustom greader-account nil "your google account name (use full form ie XXXXX@gmail.com)" :group 'greader)
(defcustom greader-password nil "your google password name" :group 'greader)

(defvar greader-client-login-url "https://www.google.com/accounts/ClientLogin"
  "base url for Google authentication")

(defvar greader-authentication-header-format "accountType=HOSTED_OR_GOOGLE&Email=%s&Passwd=%s&service=reader"
  "format for auth credential header for Google authentication")

(defvar greader-auth-token-header-format "Authorization: GoogleLogin auth=%s"
  "header format for authenticated requests")

;; once users are able to specify url retrieval program the variables
;; below should be converted to defcustom

(defvar greader-use-url-retrieve nil
  "If nil, use external command-line HTTP client instead.")

(defvar greader-url-retrieval-program "curl"
  "NOT IMPLEMENTED: URL retrieving program used when `install-elisp-use-url-retrieve' is nil.
  you may want to try or the native program \"wget -q -O- %s\"")

(defvar greader-auth-token-buffer-name "*greader auth token*"
  "this buffer is used for grabbing auth token")

(defvar greader-auth-token-string nil
  "this string will be used for all authentication requests")

;;  These are various urls needed to access the Google Reader api
(defvar greader-reading-list-url-format "http://www.google.com/reader/api/0/stream/contents/user/-/state/com.google/reading-list?xt=user/-/state/com.google/read&n=%d%s&client=scroll"
  "This url fetches unread items where %d is the number of items per fetch and %s is where the continuation string will go if any")

(defun greader-kill-token-buffer ()
  (if (get-buffer greader-auth-token-buffer-name)
      (kill-buffer greader-auth-token-buffer-name)))

(defun greader-authenticate ()
  (greader-kill-token-buffer)
  (let* ((gr-header (format greader-authentication-header-format greader-account greader-password)))
    ;;; this calls an asynchronous process ("curl, etc.") that writes output to temp buffer
    (start-process "greader-account-auth"
                   greader-auth-token-buffer-name
                   greader-url-retrieval-program
                   "--silent"
                   "-d"
                   gr-header
                   greader-client-login-url)))

(defun greader-set-auth-token ()
  (greader-authenticate)
  (set-buffer (get-buffer greader-auth-token-buffer-name))
  (goto-char (point-min))
  (re-search-forward "Auth=\\([a-zA-Z0-9-_]+\\)" nil t nil)
  (setq greader-auth-token-string (match-string 1))
  (greader-kill-token-buffer))

(defun greader-get-url (url)
  (let* ((gr-header (format greader-auth-token-header-format greader-auth-token-string)))
    (start-process "greader-get-url"
                   "temp buffer name"
                   greader-url-retrieval-program
                   "--silent"
                   "-d"
                   gr-header
                   url)))


;; the declarations below are for testing purposes
;; (greader-set-auth-token)

(provide 'greader)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; greader.el ends here










