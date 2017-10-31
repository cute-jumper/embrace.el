;;; embrace.el --- Add/Change/Delete pairs based on `expand-region'  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; Package-Requires: ((cl-lib "0.5") (expand-region "0.10.0"))
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;                              _____________

;;                                EMBRACE.EL

;;                               Junpeng Qiu
;;                              _____________


;; Table of Contents
;; _________________

;; 1 Overview
;; 2 Usage
;; .. 2.1 Example
;; .. 2.2 Screencasts
;; .. 2.3 `embrace-change' and `embrace-delete'
;; .. 2.4 `embrace-add'
;; 3 Customization
;; .. 3.1 Adding More Semantic Units
;; .. 3.2 Adding More Surrounding Pairs
;; .. 3.3 Disable Help Message
;; .. 3.4 Example Settings
;; 4 For `evil-surround' Users
;; .. 4.1 Where `embrace' is better
;; .. 4.2 Why not use together?
;; 5 Contributions
;; 6 Related Packages


;; Add/Change/Delete pairs based on [expand-region].

;; For `evil-surround' integration, see [evil-embrace].


;; [expand-region] https://github.com/magnars/expand-region.el

;; [evil-embrace] https://github.com/cute-jumper/evil-embrace.el


;; 1 Overview
;; ==========

;;   This package is heavily inspired by [evil-surround] (which is a port
;;   of the vim plugin [surround.vim]). But instead of using `evil' and its
;;   text objects, this package relies on another excellent package
;;   [expand-region].

;;   For Emacs users who don't like `evil' and thus don't use
;;   `evil-surround', `embrace' provides similar commands that can be found
;;   in `evil-surround'. `Evil' is absolutely *not* required. For
;;   `evil-surround' users, `embrace' can make your `evil-surround'
;;   commands even better! (Have you noticed that `evil-surround' doesn't
;;   work on many custom pairs?)


;; [evil-surround] https://github.com/timcharper/evil-surround

;; [surround.vim] https://github.com/tpope/vim-surround

;; [expand-region] https://github.com/magnars/expand-region.el


;; 2 Usage
;; =======

;;   There are three commands: `embrace-add', `embrace-change' and
;;   `embrace-delete' that can add, change, and delete surrounding pairs
;;   respectively. You can bind these commands to your favorite key
;;   bindings.

;;   There is also a dispatch command `embrace-commander'. After invoking
;;   `embrace-commander', you can hit:
;;   - `a' for `embrace-add'
;;   - `c' for `embrace-change'
;;   - `d' for `embrace-delete'


;; 2.1 Example
;; ~~~~~~~~~~~

;;   It might be a little hard for users who have no experience in `evil'
;;   and `evil-surround' to understand what `embrace' can do. So let's give
;;   an example to show what `embrace' can do fist. You can look at the
;;   following sections to see the meaning of key bindings. In this
;;   example, I bind C-, to `embrace-commander'. Assume we have following
;;   text in `c-mode' and the cursor position is indicated by `|':
;;   ,----
;;   | fo|o
;;   `----

;;   Press C-, a w ' to add '' to the current word:
;;   ,----
;;   | 'fo|o'
;;   `----

;;   Press C-, a q { to add {} to outside of the quotes:
;;   ,----
;;   | {'fo|o'}
;;   `----

;;   Press C-, c ' " to change the '' to "":
;;   ,----
;;   | {"fo|o"}
;;   `----

;;   Press C-, c { t, and then enter the tag: body class="page-body", to
;;   change the {} to a tag:
;;   ,----
;;   | <body class="page-body">"fo|o"</body>
;;   `----

;;   Press C-, c t f, and enter the function name `bar' to change the tag
;;   to a function call:
;;   ,----
;;   | bar("fo|o")
;;   `----

;;   Press C-, d f to remove the function call:
;;   ,----
;;   | "fo|o"
;;   `----

;;   If you're an `evil-surround' user, you might notice that the last
;;   command can't be achieved by `evil-surround'. However, it works in
;;   `embrace'! And yes, you can find even more examples in which
;;   `evil-surround' doesn't work while `embrace' works!


;; 2.2 Screencasts
;; ~~~~~~~~~~~~~~~

;;   For non `evil-mode' users, use the following settings (they will be
;;   explained later):
;;   ,----
;;   | (global-set-key (kbd "C-,") #'embrace-commander)
;;   | (add-hook 'org-mode-hook #'embrace-org-mode-hook)
;;   `----

;;   Open an org-mode file, we can perform the following pair changing:

;;   [./screencasts/embrace.gif]

;;   For `evil-mode' users, here is a similar screencast (see
;;   [evil-embrace] for more details):

;;   [https://github.com/cute-jumper/evil-embrace.el/blob/master/screencasts/evil-embrace.gif]


;; [evil-embrace] https://github.com/cute-jumper/evil-embrace.el


;; 2.3 `embrace-change' and `embrace-delete'
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   These two commands can change and delete the surround pair
;;   respectively. For `evil-surround' users, `embrace-change' is similar
;;   to `cs' and `embrace-delete' is similar to `ds'.

;;   The surrounding pair is specified by a key, which is very similar to
;;   the key used for Vim's text objects. For example, `(' stands for the
;;   surrounding pair `(' and `)', and `{' stands for the surrouding pair,
;;   `{' and `}'. The default key mappings are shown below:
;;    Key  Left             right
;;   --------------------------------
;;    (    "("              ")"
;;    )    "( "             " )"
;;    {    "{"              "}"
;;    }    "{ "             " }"
;;    [    "["              "]"
;;    ]    "[ "             " ]"
;;    >    "<"              ">"
;;    "    "\""             "\""
;;    '    "\'"             "\'"
;;    `    "`"              "`"
;;    t    "<foo bar=100>"  "</foo>"
;;    f    "func("          ")"

;;   Note that for `t' and `f' key, the real content is based on the
;;   user's input. Also, you can override the closing quote when
;;   entering a ` (backquote) in emacs-lisp to get a ' (apostrophe)
;;   instead of a ` (backquote) by using
;;   `embrace-emacs-lisp-mode-hook' (see below).


;; 2.4 `embrace-add'
;; ~~~~~~~~~~~~~~~~~

;;   This command is similar to `evil-surround''s `ys' command. We need to
;;   enter a key for the semantic unit to which we want to add a
;;   surrounding pair. The semantic unit is marked by the functions
;;   provided by `expand-region'.

;;   Here is the default mapping:
;;    key  mark function
;;   -----------------------------
;;    w    er/mark-word
;;    s    er/mark-symbol
;;    d    er/mark-defun
;;    p    er/mark-outside-pairs
;;    P    er/mark-inside-pairs
;;    q    er/mark-outside-quotes
;;    Q    er/mark-inside-quotes
;;    .    er/mark-sentence
;;    h    er/mark-paragraph

;;   After pressing a key to select the semantic unit, you can press
;;   another key to add the surrounding pair, which is the same as
;;   `embrace-change' and `embrace-delete'.


;; 3 Customization
;; ===============

;; 3.1 Adding More Semantic Units
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   You can modify the variable `embrace-semantic-units-alist' and note
;;   that this variable is buffer-local so it is better to change the value
;;   in a hook:
;;   ,----
;;   | (add-hook 'text-mode-hook
;;   |     (lambda ()
;;   |        (add-to-list 'embrace-semantic-units-alist '(?e . er/mark-email))))
;;   `----


;; 3.2 Adding More Surrounding Pairs
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   Use the command `embrace-add-pair' to add a pair:
;;   ,----
;;   | (embrace-add-pair key left right)
;;   `----

;;   The change is also buffer-local, so wrap it in a hook function:
;;   ,----
;;   | (add-hook 'LaTeX-mode-hook
;;   |     (lambda ()
;;   |        (embrace-add-pair ?e "\\begin{" "}")))
;;   `----

;;   If you want add something like the `t' key for the tag, you can look
;;   at the function `embrace-add-pair-regexp' in the source code.

;;   Note that if you're using `embrace-add-pair' to add an existing key,
;;   then it will replace the old one.


;; 3.3 Disable Help Message
;; ~~~~~~~~~~~~~~~~~~~~~~~~

;;   If you find the help message annoying, use the following code to
;;   disable it:
;;   ,----
;;   | (setq embrace-show-help-p nil)
;;   `----


;; 3.4 Example Settings
;; ~~~~~~~~~~~~~~~~~~~~

;;   I recommend binding a convenient key for `embrace-commander'. For
;;   example,
;;   ,----
;;   | (global-set-key (kbd "C-,") #'embrace-commander)
;;   `----

;;   We have defined several example hook functions that provide additional
;;   key bindings which can be used in different major modes. Right now
;;   there are hooks for `LaTeX-mode', `org-mode', `ruby-mode' (including
;;   `enh-ruby-mode') and `emacs-lisp-mode':

;;   `LaTeX-mode':
;;    Key  Left      Right
;;   ----------------------
;;    =    \verb|    |
;;    ~    \texttt{  }
;;    *    \textbf{  }

;;   `org-mode':
;;    Key  Left              Right
;;   ------------------------------------------
;;    =    =                 =
;;    ~    ~                 ~
;;    *    *                 *
;;    _    _                 _
;;    +    +                 +
;;    k    `@@html:<kbd>@@'  `@@html:</kbd>@@'

;;   `ruby-mode' and `enh-ruby-mode':
;;    Key  Left  Right
;;   ------------------
;;    #    #{     }
;;    d    do     end

;;   `emacs-lisp-mode':
;;    Key  Left  Right
;;   ------------------
;;    `    `      '

;;   To use them:
;;   ,----
;;   | (add-hook 'LaTeX-mode-hook 'embrace-LaTeX-mode-hook)
;;   | (add-hook 'org-mode-hook 'embrace-org-mode-hook)
;;   | (add-hook 'ruby-mode-hook 'embrace-ruby-mode-hook) ;; or enh-ruby-mode-hook
;;   | (add-hook 'emacs-lisp-mode-hook 'embrace-emacs-lisp-mode-hook)
;;   `----

;;   The code of two of the hooks above (which are defined in `embrace.el'):
;;   ,----
;;   | (defun embrace-LaTeX-mode-hook ()
;;   |   (dolist (lst '((?= "\\verb|" . "|")
;;   |                  (?~ "\\texttt{" . "}")
;;   |                  (?/ "\\emph{" . "}")
;;   |                  (?* "\\textbf{" . "}")))
;;   |     (embrace-add-pair (car lst) (cadr lst) (cddr lst))))
;;   | (defun embrace-org-mode-hook ()
;;   |   (dolist (lst '((?= "=" . "=")
;;   |                  (?~ "~" . "~")
;;   |                  (?/ "/" . "/")
;;   |                  (?* "*" . "*")
;;   |                  (?_ "_" . "_")
;;   |                  (?+ "+" . "+")
;;   |                  (?k "@@html:<kbd>@@" . "@@html:</kbd>@@")))
;;   |     (embrace-add-pair (car lst) (cadr lst) (cddr lst))))
;;   `----

;;   You can define and use your own hook function similar to the code
;;   above.

;;   Welcome to add some settings for more major modes.


;; 4 For `evil-surround' Users
;; ===========================

;; 4.1 Where `embrace' is better
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   From the previous example, you can see that `embrace' actually
;;   replicates all the funcionalities provided in `evil-surround' and it
;;   can even do more than `evil-surround'. Actually, they are quite
;;   different. Since `embrace' uses `expand-region' behind the scene, you
;;   can expect it to work as long as `expand-region' works. Unlike
;;   `evil-surround', which is restricted to the pre-defined text objects,
;;   `embrace' can define nearly arbitrary surrounding pairs and three core
;;   commands always work. On the contratry, you get nearly no
;;   customization in `evil-surround': custom pairs don't work in `cs' or
;;   `ds' if you don't have a corresponding text object defined (they work
;;   in `ys').

;;   *TL;DR*: `embrace' is more customizable.


;; 4.2 Why not use together?
;; ~~~~~~~~~~~~~~~~~~~~~~~~~

;;   Sure! You can make `embrace' and `evil-surround' work together. Look
;;   at [evil-embrace]!


;; [evil-embrace] https://github.com/cute-jumper/evil-embrace.el


;; 5 Contributions
;; ===============

;;   This package is still in early stage, but it is quite usable right
;;   now. More functions can be added and the evil integration is not
;;   perfect yet. Contributions are always welcome!


;; 6 Related Packages
;; ==================

;;   - [evil-embrace]
;;   - [expand-region]
;;   - [evil-surround]
;;   - [change-inner]
;;   - [smartparens]


;; [evil-embrace] https://github.com/cute-jumper/evil-embrace.el

;; [expand-region] https://github.com/magnars/expand-region.el

;; [evil-surround] https://github.com/timcharper/evil-surround

;; [change-inner] https://github.com/magnars/change-inner.el

;; [smartparens] https://github.com/Fuco1/smartparens

;;; Code:

(require 'expand-region)
(require 'cl-lib)
(require 'font-lock)

(defgroup embrace nil
  "Add/Change/Delete pairs based on `expand-region'."
  :group 'editing
  :prefix "embrace-")

(defvar embrace-show-help-p t
  "Whether we need to show the help buffer or not.")

;; faces
(defface embrace-help-key-face
  '((t . (:bold t
                :inherit font-lock-constant-face)))
  "Face for keys."
  :group 'embrace)

(defface embrace-help-separator-face
  '((t . (:inherit font-lock-comment-face)))
  "Face for separators."
  :group 'embrace)

(defface embrace-help-pair-face
  `((t . (:inverse-video t
                         :inherit font-lock-function-name-face)))
  "Face for pairs."
  :group 'embrace)

(defface embrace-help-mark-func-face
  '((t . (:inherit font-lock-function-name-face)))
  "Face for mark functions."
  :group 'embrace)

(cl-defstruct embrace-pair-struct
  key left right left-regexp right-regexp read-function help auto-newline)

(defvar embrace-semantic-units-alist '((?w . er/mark-word)
                                       (?s . er/mark-symbol)
                                       (?d . er/mark-defun)
                                       (?P . er/mark-inside-pairs)
                                       (?p . er/mark-outside-pairs)
                                       (?Q . er/mark-inside-quotes)
                                       (?q . er/mark-outside-quotes)
                                       (?. . er/mark-sentence)
                                       (?h . er/mark-paragraph))
  "Key to mark function mapping.")
(make-variable-buffer-local 'embrace-semantic-units-alist)

(defvar embrace--pairs-list nil)

(defun embrace-add-pair (key left right &optional help auto-newline)
  (assq-delete-all key embrace--pairs-list)
  (add-to-list 'embrace--pairs-list
               (cons key (make-embrace-pair-struct
                          :key key
                          :left left
                          :right right
                          :left-regexp (regexp-quote left)
                          :right-regexp (regexp-quote right)
                          :help help
                          :auto-newline auto-newline))))

(defun embrace-add-pair-regexp
    (key left-regexp right-regexp read-function &optional help auto-newline)
  (assq-delete-all key embrace--pairs-list)
  (add-to-list 'embrace--pairs-list
               (cons key (make-embrace-pair-struct
                          :key key
                          :read-function read-function
                          :left-regexp left-regexp
                          :right-regexp right-regexp
                          :help help
                          :auto-newline auto-newline))))

(defun embrace-build-help (left right)
  (concat (propertize left 'face 'embrace-help-pair-face)
          ".."
          (propertize right 'face 'embrace-help-pair-face)))

(defun embrace--setup-defaults ()
  (dolist (pair '((?\( . ("(" . ")"))
                  (?\) . ("( " . " )"))
                  (?\{ . ("{" . "}"))
                  (?\} . ("{ " . " }"))
                  (?\[ . ("[" . "]"))
                  (?\] . ("[ " . " ]"))
                  (?< . ("<" . ">"))
                  (?> . ("< " . " >"))
                  (?\" . ("\"" . "\""))
                  (?\' . ("\'" . "\'"))
                  (?` . ("`" . "`"))))
    (embrace-add-pair (car pair) (cadr pair) (cddr pair)))
  (embrace-add-pair-regexp ?t "<[^>]*?>" "</[^>]*?>" 'embrace-with-tag
                           (embrace-build-help "<tag attr>" "</tag>"))
  (embrace-add-pair-regexp ?f "\\(\\w\\|\\s_\\)+?(" ")" 'embrace-with-function
                           (embrace-build-help "function(" ")")))

(embrace--setup-defaults)
(make-variable-buffer-local 'embrace--pairs-list)

;; -------------------------------- ;;
;; Help system based on `which-key' ;;
;; -------------------------------- ;;
(defvar embrace--help-buffer-name "*embrace-help*")
(defvar embrace--help-buffer nil)
(defvar embrace--help-add-column-width 3)
(defvar embrace-help-separator " → ")

(defun embrace--char-enlarged-p (&optional _frame)
  (> (frame-char-width)
     (/ (float (frame-pixel-width)) (window-total-width (frame-root-window)))))

(defun embrace--total-width-to-text (total-width)
  (let ((char-width (frame-char-width)))
    (- total-width
       (/ (frame-fringe-width) char-width)
       (/ (frame-scroll-bar-width) char-width)
       (if (embrace--char-enlarged-p) 1 0)
       3)))

(defun embrace--get-help-buffer-max-dims ()
  (cons (round (* 0.25 (window-total-height (frame-root-window))))
        (max 0
             (embrace--total-width-to-text
              (round (* 1.0 (window-total-width (frame-root-window))))))))

(defsubst embrace--string-width (maybe-string)
  (if (stringp maybe-string) (string-width maybe-string) 0))

(defsubst embrace--max-len (keys index)
  (cl-reduce
   (lambda (x y) (max x (embrace--string-width (nth index y))))
   keys :initial-value 0))

(defun embrace--normalize-columns (columns)
  (let ((max-len (cl-reduce (lambda (a x) (max a (length x))) columns
                            :initial-value 0)))
    (mapcar
     (lambda (c)
       (if (< (length c) max-len)
           (append c (make-list (- max-len (length c)) ""))
         c))
     columns)))

(defsubst embrace--join-columns (columns)
  (let* ((padded (embrace--normalize-columns columns))
         (rows (apply #'cl-mapcar #'list padded)))
    (mapconcat (lambda (row) (mapconcat #'identity row " ")) rows "\n")))

(defun embrace--partition-list (n list)
  (let (res)
    (while list
      (setq res (cons (cl-subseq list 0 (min n (length list))) res)
            list (nthcdr n list)))
    (nreverse res)))

(defun embrace--pad-column (col-keys)
  (let* ((col-key-width  (+ embrace--help-add-column-width
                            (embrace--max-len col-keys 0)))
         (col-sep-width  (embrace--max-len col-keys 1))
         (col-desc-width (embrace--max-len col-keys 2))
         (col-width      (+ 1 col-key-width col-sep-width col-desc-width)))
    (cons col-width
          (mapcar (lambda (k)
                    (format (concat "%" (int-to-string col-key-width)
                                    "s%s%-" (int-to-string col-desc-width) "s")
                            (nth 0 k) (nth 1 k) (nth 2 k)))
                  col-keys))))

(defun embrace--list-to-columns (keys avl-lines avl-width)
  (let ((cols-w-widths (mapcar #'embrace--pad-column
                               (embrace--partition-list avl-lines keys))))
    (when (<= (apply #'+ (mapcar #'car cols-w-widths)) avl-width)
      (embrace--join-columns (mapcar #'cdr cols-w-widths)))))

(defun embrace--create-help-string-1 (keys available-lines available-width)
  (let ((result (embrace--list-to-columns
                 keys available-lines available-width))
        found prev-result)
    (if (= 1 available-lines)
        result
      (while (and (> available-lines 1)
                  (not found))
        (setq available-lines (- available-lines 1)
              prev-result result
              result (embrace--list-to-columns
                      keys available-lines available-width)
              found (not result)))
      (if found prev-result result))))

(defun embrace--create-help-string (keys)
  (let* ((max-dims (embrace--get-help-buffer-max-dims))
         (avl-lines (1- (car max-dims)))
         (avl-width (cdr max-dims)))
    (embrace--create-help-string-1 keys avl-lines avl-width)))

(defun embrace--setup-help-buffer ()
  (with-current-buffer
      (setq embrace--help-buffer
            (get-buffer-create embrace--help-buffer-name))
    (let (message-log-max)
      (toggle-truncate-lines 1)
      (message ""))
    (setq-local cursor-type nil)
    (setq-local cursor-in-non-selected-windows nil)
    (setq-local mode-line-format nil)
    (setq-local word-wrap nil)
    (setq-local show-trailing-whitespace nil)))

(defmacro embrace--show-help-buffer-defun (name body)
  (declare (indent 1))
  (let ((func (intern (format "embrace--show-%s-help-buffer" name))))
    `(defun ,func ()
       (and embrace-show-help-p
            (embrace--show-help-buffer (embrace--create-help-string
                                        ,body))))))

(defun embrace--show-help-buffer (help-string)
  (let ((alist '((window-width . (lambda (w) (fit-window-to-buffer w nil 1)))
                 (window-height . (lambda (w) (fit-window-to-buffer w nil 1))))))
    (embrace--setup-help-buffer)
    (with-current-buffer embrace--help-buffer
      (erase-buffer)
      (insert help-string)
      (goto-char (point-min)))
    (if (get-buffer-window embrace--help-buffer)
        (display-buffer-reuse-window embrace--help-buffer alist)
      (display-buffer-in-side-window
       embrace--help-buffer alist))))

(defun embrace--pair-struct-to-keys (pair-struct)
  (list (propertize (format "%c" (embrace-pair-struct-key pair-struct))
                    'face 'embrace-help-key-face)
        (propertize embrace-help-separator
                    'face 'embrace-help-separator-face)
        (or (embrace-pair-struct-help pair-struct)
            (concat
             (propertize
              (or (embrace-pair-struct-left pair-struct)
                  (embrace-pair-struct-left-regexp pair-struct))
              'face
              'embrace-help-pair-face)
             ".."
             (propertize
              (or (embrace-pair-struct-right pair-struct)
                  (embrace-pair-struct-right-regexp pair-struct))
              'face
              'embrace-help-pair-face)))))

(embrace--show-help-buffer-defun pair
  (mapcar
   (lambda (s) (embrace--pair-struct-to-keys (cdr s)))
   embrace--pairs-list))

(defun embrace--units-alist-to-keys ()
  (mapcar (lambda (pair) (list
                      (propertize (format "%c" (car pair))
                                  'face
                                  'embrace-help-key-face)
                      (propertize embrace-help-separator
                                  'face 'embrace-help-separator-face)
                      (propertize (symbol-name (cdr pair))
                                  'face
                                  'embrace-help-mark-func-face)))
          embrace-semantic-units-alist))

(embrace--show-help-buffer-defun unit
  (embrace--units-alist-to-keys))

(defvar embrace--command-keys nil)
(defun embrace--commands-to-keys ()
  (or embrace--command-keys
      (let (lst)
        (dolist (pair '(("a" . "add")
                        ("c" . "change")
                        ("d" . "delete")))
          (push (list (propertize (car pair)
                                  'face
                                  'embrace-help-key-face)
                      (propertize embrace-help-separator
                                  'face 'embrace-help-separator-face)
                      (propertize (cdr pair)
                                  'face
                                  'embrace-help-mark-func-face))
                lst))
        (setq embrace--command-keys lst))))

(embrace--show-help-buffer-defun command
  (embrace--commands-to-keys))

(defun embrace--hide-help-buffer ()
  (and (buffer-live-p embrace--help-buffer)
       (let ((win (get-buffer-window embrace--help-buffer)))
         ;; Set `quit-restore' window parameter to fix evil-embrace/#5
         (set-window-parameter
          win 'quit-restore
          (list 'window 'window (selected-window) embrace--help-buffer))
         (quit-windows-on embrace--help-buffer))))

;; ------------------- ;;
;; funcions & commands ;;
;; ------------------- ;;
(defun embrace-with-tag ()
  (let ((input (read-string "Tag: ")) tag rest)
    (when (string-match "\\([0-9a-z-]+\\)\\(.*?\\)[>]*$" input)
      (setq tag (match-string 1 input))
      (setq rest (match-string 2 input))
      (cons (format "<%s%s>" (or tag "") (or rest ""))
            (format "</%s>" (or tag ""))))))

(defun embrace-with-function ()
  (let ((fname (read-string "Function: ")))
    (cons (format "%s(" (or fname "")) ")")))

(defun embrace--expand-region-research (open close)
  (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
    (let ((expand-region-fast-keys-enabled nil))
      (save-excursion
        (when (looking-at open)
          (er/expand-region 1))
        (while (and (not (= (point) (point-min)))
                    (not (and (looking-at open)
                              (save-excursion
                                (goto-char (region-end))
                                (looking-back close nil)))))
          (er/expand-region 1))
        (when (not (and (looking-at open)
                        (save-excursion
                          (goto-char (region-end))
                          (looking-back close nil))))
          (setq mark-active nil))
        (when (use-region-p)
          (cons (region-beginning) (region-end)))))))

(defun embrace--fallback-re-search (open close)
  (let ((start (point)))
    (save-excursion
      (when (re-search-backward open nil t)
        (push-mark)
        (goto-char start)
        (when (re-search-forward close nil t)
          (cons (mark) (point)))))))

(defun embrace--get-region-overlay (open close)
  (let ((bounds (or (embrace--expand-region-research open close)
                    (embrace--fallback-re-search open close))))
    (when bounds
      (make-overlay (car bounds) (cdr bounds) nil nil t))))

(defun embrace--insert (char overlay)
  (let* ((struct (assoc-default char embrace--pairs-list))
         (auto-newline (and struct
                            (embrace-pair-struct-auto-newline struct)))
         open close)
    (if struct
        (if (functionp (embrace-pair-struct-read-function struct))
            (let ((pair (funcall (embrace-pair-struct-read-function struct))))
              (setq open (car pair))
              (setq close (cdr pair)))
          (setq open (embrace-pair-struct-left struct))
          (setq close (embrace-pair-struct-right struct)))
      (let ((char-str (char-to-string char)))
        (setq open char-str
              close char-str)))
    (unwind-protect
        (save-excursion
          (goto-char (overlay-start overlay))
          (insert open)
          (and auto-newline
               (not (looking-at-p "[[:space:]]*\n"))
               (insert "\n"))
          (goto-char (overlay-end overlay))
          (and auto-newline
               (not (looking-back "\n[[:space:]]*" nil))
               (insert "\n"))
          (insert close))
      (delete-overlay overlay))))

(defun embrace--delete (char &optional change-p)
  (let ((struct (assoc-default char embrace--pairs-list))
        open close overlay auto-newline)
    (when struct
      (setq open (embrace-pair-struct-left-regexp struct))
      (setq close (embrace-pair-struct-right-regexp struct))
      (setq overlay (embrace--get-region-overlay open close))
      (setq auto-newline (embrace-pair-struct-auto-newline struct)))
    (unless overlay
      (error "No such a pair found"))
    (unwind-protect
        (progn
          (save-excursion
            (goto-char (overlay-start overlay))
            (when (looking-at open)
              (delete-char (string-width (match-string 0))))
            (and auto-newline
                 (looking-at-p "[[:space:]]*\n")
                 (zap-to-char 1 ?\n))
            (goto-char (overlay-end overlay))
            (when (looking-back close nil)
              (backward-delete-char (string-width (match-string 0))))
            (and auto-newline
                 (looking-back "\n[[:space:]]*" nil)
                 (delete-region (match-beginning 0) (point))))
          (when change-p overlay))
      (unless change-p (delete-overlay overlay)))))

(defun embrace--change-internal (change-p)
  (let* ((char (read-char "Delete pair: "))
         (overlay (embrace--delete char change-p)))
    (and change-p
         (overlayp overlay)
         (embrace--insert (read-char "Insert pair: ") overlay))))

;;;###autoload
(defun embrace-delete ()
  (interactive)
  (embrace--show-pair-help-buffer)
  (unwind-protect
      (embrace--change-internal nil)
    (embrace--hide-help-buffer)))

;;;###autoload
(defun embrace-change ()
  (interactive)
  (embrace--show-pair-help-buffer)
  (unwind-protect
      (embrace--change-internal t)
    (embrace--hide-help-buffer)))

(defun embrace--add-internal (beg end char)
  (let ((overlay (make-overlay beg end nil nil t)))
    (embrace--insert char overlay)
    (delete-overlay overlay)))

;;;###autoload
(defun embrace-add ()
  (interactive)
  (let (mark-func)
    (unwind-protect
        (save-excursion
          ;; only ask for semantic unit if region isn't already set
          (unless (use-region-p)
            (embrace--show-unit-help-buffer)
            (setq mark-func (assoc-default (read-char "Semantic unit: ")
                                           embrace-semantic-units-alist))
            (unless (fboundp mark-func)
              (error "No such a semantic unit"))
            (funcall mark-func))
          (embrace--show-pair-help-buffer)
          (embrace--add-internal (region-beginning) (region-end)
                                 (read-char "Add pair: ")))
      (embrace--hide-help-buffer))))

;;;###autoload
(defun embrace-commander ()
  (interactive)
  (embrace--show-command-help-buffer)
  (unwind-protect
      (let ((char (read-char "Command: ")))
        (cond
         ((eq char ?a)
          (call-interactively 'embrace-add))
         ((eq char ?c)
          (call-interactively 'embrace-change))
         ((eq char ?d)
          (call-interactively 'embrace-delete))
         (t
          (error "Unknown key"))))
    (embrace--hide-help-buffer)))

;; -------- ;;
;; Bindings ;;
;; -------- ;;
;;;###autoload
(defun embrace-LaTeX-mode-hook ()
  (dolist (lst '((?= "\\verb|" . "|")
                 (?~ "\\texttt{" . "}")
                 (?/ "\\emph{" . "}")
                 (?* "\\textbf{" . "}")))
    (embrace-add-pair (car lst) (cadr lst) (cddr lst))))

(defvar embrace-org-src-block-modes nil
  "Completions for `org-mode' source block.")

(defun embrace--get-org-src-block-modes ()
  (or embrace-org-src-block-modes
      (setq embrace-org-src-block-modes
            (delete nil
                    (cl-remove-duplicates
                     (append
                      (mapcar (lambda (x) (car (last x)))
                              (cdr (plist-get
                                    (cdr (get 'org-babel-load-languages 'custom-type))
                                    :key-type)))
                      (mapcar (lambda (x)
                                (let ((mode (cdr x)))
                                  (and (not (consp mode))
                                       (setq mode (format "%s" mode))
                                       (string-match "-mode$" mode)
                                       (intern (substring mode 0 (match-beginning 0))))))
                              auto-mode-alist)))))))

(defun embrace-with-org-block ()
  (let ((block-type (completing-read
                     "Org block type: "
                     '(center comment example export justifyleft justifyright
                              quote src verse))))
    (cond ((string= block-type "src")
           (cons
            (concat (format "#+BEGIN_SRC %s"
                            (completing-read "Language: "
                                             (embrace--get-org-src-block-modes)))
                    (let ((args (read-string "Arguments: ")))
                      (unless (string= args "")
                        (format " %s" args))))
            "#+END_SRC"))
          ((string= block-type "export")
           (cons (format "#+BEGIN_EXPORT %s"
                         (completing-read "Format: "
                                          '(ascii beamer html latex texinfo)))
                 "#+END_EXPORT"))
          (t
           (setq block-type (upcase block-type))
           (cons (format "#+BEGIN_%s" block-type)
                 (format "#+END_%s" block-type))))))

;;;###autoload
(defun embrace-org-mode-hook ()
  (dolist (lst '((?= "=" . "=")
                 (?~ "~" . "~")
                 (?/ "/" . "/")
                 (?* "*" . "*")
                 (?_ "_" . "_")
                 (?+ "+" . "+")
                 (?k "@@html:<kbd>@@" . "@@html:</kbd>@@")))
    (embrace-add-pair (car lst) (cadr lst) (cddr lst)))
  (embrace-add-pair-regexp ?l "#\\+BEGIN_.*" "#\\+END_.*" 'embrace-with-org-block
                           (embrace-build-help "#+BEGIN_*" "#+END") t))


;;;###autoload
(defun embrace-ruby-mode-hook ()
  (dolist (lst '((?# "#{" "}")
                 (?d "do" "end")))
    (embrace-add-pair (car lst) (cadr lst) (cl-caddr lst))))

;;;###autoload
(defun embrace-emacs-lisp-mode-hook ()
  (embrace-add-pair ?` "`" "'"))

(provide 'embrace)
;;; embrace.el ends here
