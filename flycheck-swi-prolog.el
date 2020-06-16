;;; flycheck-swi-prolog --- Flycheck SWI Prolog checker

;; Copyright (C) 2020 Dragan Ivanovic

;; MIT Licence

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; Author: Dragan Ivanovic <idragan(at)mac(dot)com>

;; Keywords: prolog flycheck swi

;; Homepage: https://github.com/dragan-ivanovic/flycheck-swi-prolog

;; Package-Version: 0.8

;; Package-Requires: (flycheck rx)

;;; Commentary:

;; This package provides flycheck module for syntax-checking Prolog
;; files written in SWI Prolog dialect.  It has been tested with:
;;
;; - Emacs 26
;; - Flycheck 20200610.1809
;; - SWI Prolog 8.2.0

;;; Code:

(require 'flycheck)
(require 'rx)

(defconst +flycheck-swi-prolog-start-marker+
  (rx line-start (group-n 1 (or "ERROR" "Warning")) ;; Marker
      ":" (one-or-more blank)
      (group-n 2 (one-or-more not-newline)) ;; Message
      line-end)
  "Matches an error or a warning in SWI Prolog output.")

(defconst +flycheck-swi-prolog-flcr+
  (rx string-start
      (group-n 1 (one-or-more (not (any ?:)))) ;; File
      ":"
      (optional (group-n 2 (one-or-more digit)) ;; Line
		":"
		(optional (group-n 3 (one-or-more digit)) ;; Column
			  ":"))
      (optional (one-or-more blank)
		(group-n 4 (one-or-more any))) ;; Rest
      string-end)
  "Matches a message of the form `file:[line:[column:]][ rest]'.")

(defconst +flycheck-swi-prolog-prolog-source-file+
  (rx ".pl" (optional "t") string-end)
  "Matches file names ending in .pl and .plt.")

(defconst +flycheck-swi-prolog-cont-marker+
  (rx line-start (group-n 1 (or "ERROR" "Warning")) ":"
      blank (one-or-more blank)
      (group-n 2 (zero-or-more not-newline)) ;; Message
      line-end)
  "Matches message continuation.")

(defconst +flycheck-swi-prolog-module-pattern+
  (rx line-start (zero-or-more blank)
      ":-"
      (zero-or-more blank)
      (group-n 1 "module")
      "(")
  "Matches the start of 'module' directive.")

(defun flycheck-swi-prolog-level (marker-string)
  "Map MARKER-STRING to a symbol, ERROR, WARNING, or INFO."
  (cond
   ((string= marker-string "ERROR")   'error)
   ((string= marker-string "Warning") 'warning)
   (t 'info)))

(defun flycheck-swi-prolog-parse-messages (output checker buffer)
  "Parse the checker OUTPUT into a list of messages.
Additional arguments are CHECKER and BUFFER."
  (let ((result nil)
	(i 0))
    (while (string-match +flycheck-swi-prolog-start-marker+ output i)
      (setq i (match-end 0)) ;; Move past the match
      (let ((marker (match-string 1 output)) ;; Extract the marker
	    (message (match-string 2 output)) ;; Extract the message
	    file-name
	    line-number
	    column-number)
	;; Check if the message has the format:
	;; <file> ":" [ line-number ":" [ column-number ":" ]] <rest>
	(when (and message
		   (string-match +flycheck-swi-prolog-flcr+ message))
	  (setq file-name (match-string 1 message))
	  (setq line-number (and (match-beginning 2)
				 (string-to-number (match-string 2 message))))
	  (setq column-number (and (match-beginning 3)
			     (string-to-number (match-string 3 message))))
	  ;; This is to fight ambiguity when the message contains ":"
	  ;; but it is not followed by a line number.  To interpret
	  ;; the part preceding ":" as a valid file name, it has to
	  ;; end in .pl or .plt.
	  (when (or line-number
		    (and file-name
			 (string-match-p +flycheck-swi-prolog-prolog-source-file+ file-name)))
	    (setq message (match-string 4 message))))
	;; If message is nil, it continues in the next line.
	(unless message
	  (while (and (string-match +flycheck-swi-prolog-cont-marker+ output i)
		      (= (1+ i) (match-beginning 0))
		      (string= (match-string 1 output) marker))
	    (setq i (match-end 0))
	    (when (match-beginning 2)
	      (setq message
		    (if message
			(concat message "\n" (match-string 2 output))
		      (match-string 2 output))))))
	;; If line number is nil, look for the module declaration.
	(unless line-number
	  (with-current-buffer (get-buffer-create buffer)
	    (save-excursion
	      (goto-char (point-min))
	      (when (re-search-forward +flycheck-swi-prolog-module-pattern+ (point-max) t)
		(setq line-number (line-number-at-pos))))))
	;; Append to the result
	(setq result
	      (append result
		      (list (flycheck-error-new :filename file-name
						:line line-number
						:column column-number
						:message message
						:level (flycheck-swi-prolog-level marker)
						:buffer buffer
						:checker checker))))))
    result))

;;###autoload
(flycheck-define-checker
 prolog
 "A Prolog Syntax Checker using SWI Prolog Compiler."
 :command ("swipl" "-q" "-t" "halt" "-s" source)
 :error-parser flycheck-swi-prolog-parse-messages
 :modes prolog-mode)

(provide 'flycheck-swi-prolog)

;;; flycheck-swi-prolog.el ends here

