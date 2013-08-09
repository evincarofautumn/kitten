(defvar kitten-mode-hook nil)

(defvar kitten-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for Kitten major mode")

(add-to-list 'auto-mode-alist '("\\.ktn\\'" . kitten-mode))

(defvar kitten-font-lock-keywords
  (list
   '("__[0-9a-z_]+" . font-lock-builtin-face)
   '("\\<\\(true\\|false\\)\\>" . font-lock-constant-face)
   '("\\<\\([A-Z][0-9A-Za-z_]*\\)\\>" . font-lock-type-face)
   '("\\s_+" . font-lock-variable-name-face)
   '("\\<\\(choice\\|def\\|else\\|from\\|i\\(f\\|mport\\)\\|option\\|to\\)\\>" . font-lock-keyword-face)
   '("\\([a-z][0-9A-Za-z_]*\\)" 1 font-lock-default-face t))
  "Default highlighting for Kitten mode")

; (defun kitten-indent-line ()
;   "Indent current line as Kitten code"
;   (interactive))

(defvar kitten-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ "_ 124b" table)
    (modify-syntax-entry ?* "_ 23" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?! "_" table)
    (modify-syntax-entry ?# "_" table)
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?% "_" table)
    (modify-syntax-entry ?& "_" table)
    (modify-syntax-entry ?+ "_" table)
    (modify-syntax-entry ?- "_" table)
    (modify-syntax-entry ?. "_" table)
    (modify-syntax-entry ?; "_" table)
    (modify-syntax-entry ?< "_" table)
    (modify-syntax-entry ?= "_" table)
    (modify-syntax-entry ?> "_" table)
    (modify-syntax-entry ?? "_" table)
    (modify-syntax-entry ?@ "_" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?^ "_" table)
    (modify-syntax-entry ?| "_" table)
    (modify-syntax-entry ?~ "_" table)
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?' "\"" table)
    table)
  "Syntax table for Kitten mode")

(defun kitten-mode ()
  "Major mode for editing Kitten source files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table kitten-mode-syntax-table)
  (use-local-map kitten-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(kitten-font-lock-keywords))
  ; (set (make-local-variable 'indent-line-function) 'kitten-indent-line)
  (setq major-mode 'kitten-mode)
  (setq mode-name "Kitten")
  (run-hooks 'kitten-mode-hook))

(provide 'kitten-mode)
