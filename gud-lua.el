;;; gud-lua.el --- Debug lua with debugger.lua and GUD -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/gud-lua
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Created: 17 November 2023
;; Keywords: tools, languages, debugging, gud, lua

;; This file is not part of GNU Emacs.
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

;;; Commentary:
;;
;; GUD support for debugging lua programs instrumented using breakpoints with
;; debugger.lua from https://github.com/slembcke/debugger.lua.
;;
;;; Code:

(require 'gud)

(defcustom gud-lua-command-name "lua"
  "Command to run lua program."
  :type 'string
  :group 'gud)

(defcustom gud-lua-prompt-regexp "^debugger.lua> *"
  "Regexp to match debbuger.lua prompt."
  :type 'string
  :group 'gud)

(defvar gud-lua-repeat-map
  (let ((map (make-sparse-keymap)))
    (pcase-dolist (`(,key . ,cmd) '(("n" . gud-next)
                                    ("s" . gud-step)
                                    ("c" . gud-cont)
                                    ("f" . gud-finish)
                                    ("<" . gud-up)
                                    (">" . gud-down)
                                    ("l" . gud-refresh)))
      (define-key map key cmd))
    map)
  "Keymap to repeat `gud-lua' stepping instructions.
Used in `repeat-mode'.")

;; Byte-compiler complains about unknown functions
(eval-when-compile
  (defmacro gud-lua--declare (&rest fns)
    (macroexp-progn
     `(,@(mapcar (lambda (fn)
                   `(declare-function
                     ,(intern (format "gud-%s" (symbol-name fn))) "gud" (arg)))
                 fns))))
  (gud-lua--declare next step cont finish up down refresh trace statement print))

;; Match possibilities:
;; break via dbg() => file.lua:123 in ...
;; break via dbg.call() => file.lua:123 in ...
;; Inspecting frame: ./debugger.lua:505 in upvalue 'dbg'
;; ./debugger.lua:502 in upvalue 'dbg'
(defvar gud-lua-marker-regexp
  (concat
   "^\\(?:break via dbg\\(?:[.]call\\)?([^\n\r]*) => \\([[:graph:]]+\\):\\([0-9]+\\)\\|"
   "\\(?:Inspecting frame: \\)?\\([[:graph:]]+\\):\\([0-9]+\\) in\\)[^\n\r]*"))

(defvar gud-lua-marker-regexp-start "^\\(?:break\\|Inspecting frame:\\)")

(defvar-local gud-lua-marker-acc "")

;; Notes from gud.el
;; There's no guarantee that Emacs will hand the filter the entire
;; marker at once; it could be broken up across several strings.  We
;; might even receive a big chunk with several markers in it.  If we
;; receive a chunk of text which looks like it might contain the
;; beginning of a marker, we save it here between calls to the
;; filter.
(defun gud-lua-marker-filter (string)
  (setq gud-lua-marker-acc (concat gud-lua-marker-acc string))
  (let ((output ""))
    ;; Process all the complete markers in this chunk.
    (while (string-match gud-lua-marker-regexp gud-lua-marker-acc)
      (let ((file (or (match-string 1 gud-lua-marker-acc)
                      (match-string 3 gud-lua-marker-acc)))
            (line (string-to-number
                   (or (match-string 2 gud-lua-marker-acc)
                       (match-string 4 gud-lua-marker-acc)))))
        ;; Extract the frame position from the marker.
        (setq gud-last-frame (cons file line)
              ;; Output everything instead of the below
              output (concat output (substring gud-lua-marker-acc 0 (match-end 0)))
              ;; Set the accumulator to the remaining text.
              gud-lua-marker-acc (substring gud-lua-marker-acc (match-end 0)))))
    ;; Does the remaining text look like it might end with the
    ;; beginning of another marker?  If it does, then keep it in
    ;; gud-lua-marker-acc until we receive the rest of it.  Since we
    ;; know the full marker regexp above failed, it's pretty simple to
    ;; test for marker starts.
    (if (string-match gud-lua-marker-regexp-start gud-lua-marker-acc)
        ;; Everything before the potential marker start can be output.
        (setq output (concat output (substring gud-lua-marker-acc 0 (match-beginning 0)))
              ;; Everything after, we save, to combine with later input.
              gud-lua-marker-acc (substring gud-lua-marker-acc (match-beginning 0)))

      (setq output (concat output gud-lua-marker-acc)
            gud-lua-marker-acc ""))

    output))

;;; TODO:
;; define `gud-find-expr-function' to get lua expression/statement at point for
;; print/eval
;; See default `gud-find-c-expr'

;;;###autoload
(defun gud-lua (command-line)
  "Run lua program instrumented with debugger.lua breakpoints.
The program is run with command line COMMAND-LINE in buffer named `*gud-FILE*'.

The directory containing the lua program becomes the initial working
directory and source-file directory for your debugger.

For general information about commands available to control the debugging
process from gud, see `gud-mode'."
  (interactive (list (gud-query-cmdline 'lua)))
  (gud-common-init command-line nil 'gud-lua-marker-filter)
  (setq-local gud-minor-mode 'gud-lua)

  (gud-def gud-step      "s"    "\C-s" "Step one line (into functions)."    )
  (gud-def gud-next      "n"    "\C-n" "Step one line (skip functions)."    )
  (gud-def gud-cont      "c"    "\C-r" "Continue running program."          )
  (gud-def gud-finish    "f"    "\C-f" "Finish executing current function." )
  (gud-def gud-up        "u"    "<"    "Up stack frame."                    )
  (gud-def gud-down      "d"    ">"    "Down stack frame."                  )
  (gud-def gud-print     "p %e" "\C-p" "Evaluate lua expression at point."  )
  (gud-def gud-statement "e %e" "\C-e" "Execute lua statement at point."    )
  (gud-def gud-trace     "t"    "\C-t" "Print stack trace."                 )

  (gud-set-repeat-map-property 'gud-lua-repeat-map)

  (setq comint-prompt-regexp gud-lua-prompt-regexp)
  (setq paragraph-start comint-prompt-regexp)
  (run-hooks 'gud-lua-mode-hook))

(provide 'gud-lua)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; gud-lua.el ends here
