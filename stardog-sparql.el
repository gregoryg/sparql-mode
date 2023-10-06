;;; TODO: fix pre-code comments
;;; sparql-mode.el --- Edit and interactively evaluate SPARQL queries.

;; Copyright (C) 2023       Gregory Grubbs

;; Author: Greg Grubbs <gregory.grubbs at gmail dot com>
;; Homepage: https://github.com/gregoryg/sparql-mode
;; Version: 0.0.0

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with SPARQL mode. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Extenstions to sparql-mode for use with Stardog

;;; Usage:

;; Add to your Emacs config:
;; TBD

;;; Code:
;; TODO: create function to change just the database
;; TODO: create function select database after querying server ( /admin/databases)
(defun gjg/handle-stardog-result (status &optional output-buffer)
  "Handle it!"
  (message "Status is %s" status)
  (when (zerop (buffer-size))
    (error "URL '%s' is not accessible" (url-recreate-url url-current-object)))
  (let ((results-buffer (current-buffer))
        (response (url-http-parse-response)))
    ;; (with-current-buffer (get-buffer-create "zgrego")
    ;; (with-temp-buffer
    (with-current-buffer output-buffer
      (let ((buffer-read-only nil))
        (erase-buffer)
        (if (and (<= 200 response) (<= response 299))
            (url-insert results-buffer)
          (insert-buffer-substring results-buffer))
        ;; (message "I got these databases %s" response ))
        (goto-char 0)
        (completing-read "Ohai: "
                         (append (cdr (assq 'databases (json-read))) nil))))))

(defun gjg/stardog-get-databases (server)
  "Get list of databases from the Stardog server - will work for users with admin rights"
  ;; (url-retrieve (concat server "/admin/databases") #'gjg/handle-stardog-result )
  (url-retrieve (concat (replace-regexp-in-string "/+$" "" server) "/admin/databases") #'gjg/handle-stardog-result (list (current-buffer)))
  (url-retrieve (concat (replace-regexp-in-string "/+$" "" server) "/admin/databases") #'gjg/handle-stardog-result))

(defun gjg/stardog-build-url (arg &optional server database api-endpoint reasoning  )
  "Create a Stardog specific URL for use with Stardog Platform API.  Support query and update endpoints as well as reasoning"
  (interactive "P")
  (let* ((server (or server
                  (cdr (assoc :server (gjg/set-sparql-headers)))))
         (database (or database (completing-read "Database name: " '("this" "thag"))))
         (api-endpoint (if arg "update" "query")))
    (message
    (format "%s/%s/%s"
            server
            database
            api-endpoint
            (when (and (not arg) (string= "query" api-endpoint)) "?reasoning=true")))))

(defun gjg/set-sparql-header-args (url db endpoint)
  "Set local buffer header args for SPARQL."
  (let* ((url (replace-regexp-in-string "//+$" "" url))
         (fullurl (concat url "/" db "/" endpoint "/" )))
    ;; (message "My lovely url is %s\n" fullurl)
    (let ((myheader
           (add-to-list 'org-babel-default-header-args:sparql
                        `(:server . url )
                        ))))
    (setq-local org-babel-default-header-args:sparql
                (cons `(:server . ,url)
                      (cons `(:url  . ,fullurl)
                            (assq-delete-all :url org-babel-default-header-args:sparql))))))

(defun gjg/set-stardog-bash-header-args (tramp-path )
  (setq-local org-babel-default-header-args:bash
              (cons `(:dir . ,tpath)
                    (assq-delete-all :dir org-babel-default-header-args:bash))))

(defun gjg/annotate-sparql-selection (s)
  "Provide annotations for completing-read using the data in a SPARQL Stardog server table"
  ;; (message (format "DEBUG: stardog list |%s|, table |%s}" s minibuffer-completion-table))
  (let* ((item (assoc (intern s) minibuffer-completion-table))
         (connection-name (symbol-name (nth 0 item)))
         (displayurl (s-truncate 50 (nth 1 item)))
         (db (nth 2 item))
         )
    (when item (concat
                (string-pad "" (- 20 (string-width connection-name)))
                displayurl
                (string-pad "" (- 55 (string-width displayurl)))
                db))))

(defun gjg/set-sparql-headers ()
  "Set Org Babel headers for SPARQL.   table named `svar` should have these columns: connection-name, base-url, database, api-type"
  (interactive)
  (let* ((marginalia-align-offset 80)
         (completion-extra-properties '(:annotation-function gjg/annotate-sparql-selection))
         (myconnection (assoc (intern (completing-read "SPARQL Server name: " gjg/stardog-connections-alist)) gjg/stardog-connections-alist))
         (connection-name (nth 0 myconnection))
         (url (nth 1 myconnection))
         (db (nth 2 myconnection))
         (api-type "query"))
    ;; (message (format "I will surely set sparql header args to %s %s %s %s" connection-name url db api-type))
    (gjg/set-sparql-header-args url db api-type)))
