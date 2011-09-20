;;; ash-mode.el --- ASH mode derived from CC Mode

;; Author:     2009 Dylan R. E. Moonfire
;; Maintainer: Dylan R. E. Moonfire <contact@mfgames.com>
;; Created:    2009-09-20
;; Modified:   2009-09-22
;; Version:    0.3.0
;; Keywords:   ash languages oop

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;;    This is a separate mode to implement the ASH constructs and
;;    font-locking. It is based on the java-mode example from cc-mode.
;;
;;    Note: The interface used in this file requires CC Mode 5.30 or
;;    later.

;;; .emacs (don't put in (require 'ash-mode))
;; (autoload 'ash-mode "ash-mode" "Major mode for editing ASH code." t)
;; (setq auto-mode-alist
;;    (append '(("\\.ash$" . ash-mode)) auto-mode-alist))

;; This is based on CC mode, so we need to include this.
(require 'cc-mode)

;; These are only required at compile time to get the sources for the
;; language constants.  (The cc-fonts require and the font-lock
;; related constants could additionally be put inside an
;; (eval-after-load "font-lock" ...) but then some trickery is
;; necessary to get them compiled.)
(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))

(eval-and-compile
  ;; Make our mode known to the language constant system.  Use Java
  ;; mode as the fallback for the constants we don't change here.
  ;; This needs to be done also at compile time since the language
  ;; constants are evaluated then.
  (c-add-language 'ash-mode 'c-mode))


;; Set up the basic syntax table for processing.
;;(defvar ash-mode-syntax-table nil
;;  "Syntax table used in ash-mode buffers.")


;; Create the syntax table for this mode.
;; (defvar ash-mode-syntax-table nil
;;   "Syntax table used in ash-mode buffers.")
;; (or ash-mode-syntax-table
;;     (setq ash-mode-syntax-table
;; 	  (funcall (c-lang-const c-make-mode-syntax-table ash))))

;; Sets up the syntax table which is what determines what is a string
;; literal or what should be treated as a comment.
(defvar ash-mode-syntax-table
  (let ((table (make-syntax-table)))
	(modify-syntax-entry ?_  "_"        table)
	(modify-syntax-entry ?\\ "\\"       table)
	(modify-syntax-entry ?+  "."        table)
	(modify-syntax-entry ?-  "."        table)
	(modify-syntax-entry ?=  "."        table)
	(modify-syntax-entry ?%  "."        table)
	(modify-syntax-entry ?<  "."        table)
	(modify-syntax-entry ?>  "."        table)
	(modify-syntax-entry ?&  "."        table)
	(modify-syntax-entry ?|  "."        table)
	(modify-syntax-entry ?\' "\""       table)
	(modify-syntax-entry ?\240 "."      table)

	;; ASH needs to treat [] as string literals. Sadly, since all [] aren't
	;; string characters, we can't do this in a syntax table.
	(modify-syntax-entry ?[  "|"        table)
						 (modify-syntax-entry ?]  "|"        table)

	;; Sadly, Emacs doesn't support three comment styles which is required
	;; to handle ASH's third comment (#)'s to work.
	;;(modify-syntax-entry ?#  "."        table)

	;; This sets up the block command and forward commands.
	(cond
	 ;; XEmacs
	 ((memq '8-bit c-emacs-features)
	  (modify-syntax-entry ?/  ". 1456" table)
	  (modify-syntax-entry ?*  ". 23"   table))
	 ;; Emacs
	 ((memq '1-bit c-emacs-features)
	  (modify-syntax-entry ?/  ". 124b" table)
	  (modify-syntax-entry ?*  ". 23"   table))
	 ;; Other
	 (t (error "CC Mode is incompatible with this version of Emacs")))

	;; Handle newlines to end the EOL comments.
	(modify-syntax-entry ?\n "> b"  table)
	(modify-syntax-entry ?\^m "> b" table)
	
	table)
  "Syntax table used while in `ash-mode'.")

(defvar ash-mode-abbrev-table nil
  "Abbreviation table used in ash-mode buffers.")
(c-define-abbrev-table 'ash-mode-abbrev-table
  ;; Keywords that if they occur first on a line might alter the
  ;; syntactic context, and which therefore should trig reindentation
  ;; when they are completed.
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
	))

(defvar ash-mode-map (let ((map (c-make-inherited-keymap)))
					   ;; Add bindings which are only useful for ASH
					   map)
  "Keymap used in ash-mode buffers.")

;; ASH treats the "$type[content]" as effectively a constant. In
;; specific, content should be treated as a string. This regex is used
;; to catch that change before we do invalid string processing.
(defconst ash-mode-item-lookup-regex
  "$\\w\\[\\(.+\\)\\]")

;; Formatting the include needs slightly different rules than the basic
;; matchers since it doesn't always terminate with a semicolon and it uses
;; angled brackets for the lines.
(c-lang-defconst c-cpp-matchers
  ash `(
		;; Set up our regexp for non-continued lines.
		,@(let* ((noncontinued-line-end "\\(\\=\\|\\(\\=\\|[^\\]\\)[\n\r]\\)")
				 (ncle-depth (regexp-opt-depth noncontinued-line-end))
				 (sws-depth (c-lang-const c-syntactic-ws-depth))
				 (nsws-depth (c-lang-const c-nonempty-syntactic-ws-depth)))
			
			`(
              ;; Fontify filenames in #include <...> as strings.
              ,@(when (c-lang-const c-cpp-include-directives)
                  (let* ((re (c-make-keywords-re nil
                               (c-lang-const c-cpp-include-directives)))
                         (re-depth (regexp-opt-depth re)))
                    `((,(concat noncontinued-line-end
                                "import"
                                (c-lang-const c-syntactic-ws)
                                "\\(<[^>\n\r]*>?\\)")
                       (,(+ ncle-depth re-depth sws-depth 1)
                        font-lock-string-face)

                       ;; Use an anchored matcher to put paren syntax
                       ;; on the brackets.
                       (,(byte-compile
                          `(lambda (limit)
                             (let ((beg (match-beginning
                                         ,(+ ncle-depth re-depth sws-depth 1)))
                                   (end (1- (match-end ,(+ ncle-depth re-depth
                                                           sws-depth 1)))))
                               (if (eq (char-after end) ?>)
                                   (progn
                                     (c-mark-<-as-paren beg)
                                     (c-mark->-as-paren end))
                                 (c-clear-char-property beg 'syntax-table)))
                             nil)))))))

			  ;; Fontify the directive names themselves
              (,(c-make-font-lock-search-function
                 (concat noncontinued-line-end
                         "\\(import\\)")
                 `(,(1+ ncle-depth) c-preprocessor-face-name t)))
			  ))
		
		;; Make hard spaces visible through an inverted 
		;; `font-lock-warning-face'.
		(eval . (list
				 "\240"
				 0 (progn
					 (unless (c-face-name-p 'c-nonbreakable-space-face)
					   (c-make-inverse-face 'font-lock-warning-face
											'c-nonbreakable-space-face))
					 ''c-nonbreakable-space-face)))		
		))

;; We need a bit of complexity with the matchers before because ASH treats
;; [] as strings but only for $properties, such as $item[bob's toy], but normal
;; maps require variables as normal.
(c-lang-defconst c-basic-matchers-before
  ash `(
		;; Handle the item lookup requests.
		;; TODO This doesn't quite work properly.
		;;,`(,ash-mode-item-lookup-regex 1 font-lock-string-face)

		;; Put a warning face on the opener of unclosed strings
		;; that can't span lines (i.e. all strings in ASH).
		,(c-make-font-lock-search-function
		  ;; Match a char before the string starter to make
		  ;; `c-skip-comments-and-strings' work correctly.
		  (concat ".\\(" c-string-limit-regexp "\\)")
		  '((c-font-lock-invalid-string)))
		
		;; Fontify keyword constants.
		,@(when (c-lang-const c-constant-kwds)
			(let ((re (c-make-keywords-re nil (c-lang-const c-constant-kwds))))
			  `((eval . (list ,(concat "\\<\\(" re "\\)\\>")
							  1 c-constant-face-name)))))

		;; Fontify all keywords except the primitive types.
		,`(,(concat "\\<"
					(c-lang-const c-regular-keywords-regexp))
		   1 font-lock-keyword-face)
		
		;; Fontify leading identifiers in fully qualified names like
		(eval . (list "\\(!\\)[^=]" 1 c-negation-char-face-name))
		))

;; ASH uses the following assignment operators
(c-lang-defconst c-assignment-operators
  ash '("="))

;; This defines the primative types for ASH
(c-lang-defconst c-primitive-type-kwds
  ash '("string" "int" "boolean" "float" "string"
		"buffer" "matcher"
		"item" "effect" "class" "stat" "skill" "familiar"
		"slot" "location" "zodiac" "monster" "element"
		"void"))

;; Define the keywords that can have something following after them.
(c-lang-defconst c-class-decl-kwds
  ash '("record"))

;; Flow control statements that don't use a () after it like
;; `if ()`.
(c-lang-defconst c-block-stmt-1-kwds
  ash '("repeat" "else"))

;; Flow control statements that have a () after it, such as `if () {}`.
(c-lang-defconst c-block-stmt-2-kwds
  ash '("if" "while" "until" "switch"))

;; This is for statements that allow for statements within the parens, but
;; it also gets the "foreach" and "for" keywords working outside of the
;; "normal" for statement. ASH doesn't have parens in their for and foreach
;; statements.
(c-lang-defconst c-paren-stmt-kwds
  ash '("for" "foreach" "cli_execute"))

;; ASH doesn't have 'goto'
(c-lang-defconst c-before-label-kwds
  ash nil)

;; Simple statements followed by an expression.
(c-lang-defconst c-simple-stmt-kwds
  ash '("return" "continue" "break" "exit"
		"new"
		"remove"
		"contains"
		"call"   ;; The generic function "pointer" in ASH.
		"notify" ;; Notifies the person of script usage;
		"script" ;; Not entirely sure the format.
		))

;; Constant keywords
(c-lang-defconst c-constant-kwds
  ash '("true" "false"))

;; Keywords in the middle of statements
(c-lang-defconst c-other-kwds
  ash '("from" "upto" "downto" "by" "in" "to"))

;; Records allow the "." accessing, like Java.
(c-lang-defconst c-identifier-ops
  ash '((right-assoc ".")))

;; We have no pragmas or warnings
(c-lang-defconst c-opt-cpp-prefix
  ash "nocluewhyIcantremovethis")

;; Set up the pragmas, or operations that don't need a semicolon at the
;; end of them, such as "include".
(c-lang-defconst c-opt-cpp-start
  ash "\\(import\\)")

(c-lang-defconst c-opt-cpp-include-directives
  ash "import")

;; ASH doesn't have #define or anything related to that.
(c-lang-defconst c-opt-macro-define
  ash nil)

;; ASH has a typedef command.
(c-lang-defconst c-typedef-decl-kwds
  ash '("typedef" "record"))

;; This almost works, but we have the problem that it still treats
;; the contents as source code when we want it to just be an
;; arbitrary string.
;;(c-lang-defconst c-other-block-decl-kwds
;;  ash '("cli_execute"))

(defcustom ash-font-lock-extra-types nil
  "*List of extra types (aside from the type keywords) to recognize in ASH mode.
Each list item should be a regexp matching a single identifier.")

(defconst ash-font-lock-keywords-1 (c-lang-const c-matchers-1 ash)
  "Minimal highlighting for ASH mode.")

(defconst ash-font-lock-keywords-2 (c-lang-const c-matchers-2 ash)
  "Fast normal highlighting for ASH mode.")

(defconst ash-font-lock-keywords-3 (c-lang-const c-matchers-3 ash)
  "Accurate normal highlighting for ASH mode.")

(defvar ash-font-lock-keywords ash-font-lock-keywords-3
  "Default expressions to highlight in ASH mode.")


;; Set up our mode hook for customizations.
(defcustom ash-mode-hook nil
  "*Hook called by `ash-mode'."
  :type 'hook
  :group 'c)

(easy-menu-define ash-menu ash-mode-map "ASH Mode Commands"
  ;; Can use `ash' as the language for `c-mode-menu'
  ;; since its definition covers any language.  In
  ;; this case the language is used to adapt to the
  ;; nonexistence of a cpp pass and thus removing some
  ;; irrelevant menu alternatives.
  (cons "ASH" (c-lang-const c-mode-menu ash)))

;;; The entry point into the mode
(defun ash-mode ()
  "Major mode for editing ASH (a scripting language used in KoLmafia) code.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `ash-mode-hook'.

Key bindings:
\\{ash-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table ash-mode-syntax-table)
  (setq major-mode 'ash-mode
		mode-name "ASH"
		local-abbrev-table ash-mode-abbrev-table
		abbrev-mode t)
  (use-local-map c-mode-map)
  (c-init-language-vars ash-mode)
  (c-common-init 'ash-mode)
  (easy-menu-add ash-menu)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'ash-mode-hook)
  (c-update-modeline))


(provide 'ash-mode)

;;; ash-mode.el ends here
