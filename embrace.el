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
;; .. 2.2 `embrace-change' and `embrace-delete'
;; .. 2.3 `embrace-add'
;; 3 Customization
;; .. 3.1 Adding More Semantic Units
;; .. 3.2 Adding More Surrounding Pairs
;; .. 3.3 Example Settings
;; 4 For `evil-surround' Users
;; .. 4.1 Where `embrace' is better
;; .. 4.2 Where `evil-surround' is better
;; .. 4.3 Why not use together?
;; 5 Contributions
;; 6 Related Packages


;; Add/Change/Delete pairs based on [expand-region].


;; [expand-region] https://github.com/magnars/expand-region.el


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


;; 2.2 `embrace-change' and `embrace-delete'
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
;;    t    "<foo bar=100>"  "</foo>"
;;    f    "func("          ")"

;;   Note that for `t' and `f' key, the real content is based on the user's
;;   input.


;; 2.3 `embrace-add'
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


;; 3.3 Example Settings
;; ~~~~~~~~~~~~~~~~~~~~

;;   I recommend binding a convenient key for `embrace-commander'. For
;;   example,
;;   ,----
;;   | (global-set-key (kbd "C-,") #'embrace-commander)
;;   `----

;;   We have defined several example hook functions that provide additional
;;   key bindings which can be used in different major modes. Right now
;;   there are hooks for `LaTeX-mode' and `org-mode':

;;   `LaTeX-mode':
;;    Key  Left      Right
;;   ----------------------
;;    =    \verb |   |
;;    ~    \texttt{  }
;;    *    \textbf{  }

;;   `org-mode':
;;    Key  Left  Right
;;   ------------------
;;    =    =     =
;;    ~    ~     ~
;;    *    *     *
;;    _    _     _
;;    +    +     +

;;   To use them:
;;   ,----
;;   | (add-hook 'LaTeX-mode-hook 'embrace-LaTeX-mode-hook)
;;   | (add-hook 'org-mode-hook 'embrace-org-mode-hook)
;;   `----

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


;; 4.2 Where `evil-surround' is better
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   `expand-region' works on semantic units, which can be different in
;;   different major modes, which causes `embrace' to have different
;;   behaviors in differnt major modes. However, `evil-surround' is based
;;   on text objects. You can expect it to have the same behavior in
;;   different major modes.

;;   Assume the following text in `LaTeX-mode':
;;   ,----
;;   | a<foo>{ ba|r }</foo>a
;;   `----

;;   Using `embrace', it is impossible to find the tag `<foo>' and `</foo>'
;;   since they are not meaningfun under `LaTeX-mode' so `expand-region'
;;   would not consider the tag as a semantic unit. Therefore,
;;   `embrace-delete' can not delete the tag (it *does* work if you delete
;;   the letter =a=s at both ends).

;;   However, if you change the major mode to `html-mode', `embrace' works
;;   since now the `<foo>' tag becomes meaningful in current context.

;;   In both situations, `evil-surround' can work because `tag' is an evil
;;   text object that has already been defined.

;;   If you're a programmer, you probably always work on some blocks that
;;   are meaningful in the current context. From this point of view,
;;   `embrace''s behavior makes more sense.


;; 4.3 Why not use together?
;; ~~~~~~~~~~~~~~~~~~~~~~~~~

;;   Sure! You can make `embrace' and `evil-surround' work together.

;;   Use the following command to enable the integration:
;;   ,----
;;   | (embrace-enable-evil-surround-integration)
;;   `----

;;   And use `embrace-disable-evil-surround-integration' to disable.

;;   The idea is that `evil-surround' works great if there are already text
;;   objects defined. So when users press a key that can be mapped to a
;;   text object, it is handled by `evil-surround'. Otherwise, let
;;   `embrace' handle it.

;;   The keys that are processed by `evil-surround' are saved in the
;;   variable `embrace-evil-surround-key'. The default value is:
;;   ,----
;;   | (?\( ?\[ ?\{ ?\) ?\] ?\} ?\" ?\' ?b ?B ?t)
;;   `----

;;   Note that this variable is also buffer-local. You should change it in
;;   the hook:
;;   ,----
;;   | (add-hook 'LaTeX-mode-hook
;;   |     (lambda ()
;;   |        (add-to-list 'embrace-evil-surround-key ?o)))
;;   `----

;;   Only these keys saved in the variable are processed by
;;   `evil-surround', and all the other keys will be processed by
;;   `embrace'. You can customize `embrace' in the way described in the
;;   previous *Customization* section to add support for additional pairs.


;; 5 Contributions
;; ===============

;;   This package is still in early stage, but it is quite usable right
;;   now. More functions can be added and the evil integration is not
;;   perfect yet. Contributions are always welcome!


;; 6 Related Packages
;; ==================

;;   - [expand-region]
;;   - [evil-surround]
;;   - [change-inner]
;;   - [smartparens]


;; [expand-region] https://github.com/magnars/expand-region.el

;; [evil-surround] https://github.com/timcharper/evil-surround

;; [change-inner] https://github.com/magnars/change-inner.el

;; [smartparens] https://github.com/Fuco1/smartparens

;;; Code:

(require 'expand-region)
(require 'cl-lib)

(cl-defstruct embrace-pair-struct
  key left right left-regexp right-regexp read-function)

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

(defun embrace-add-pair (key left right)
  (assq-delete-all key embrace--pairs-list)
  (add-to-list 'embrace--pairs-list
               (cons key (make-embrace-pair-struct
                          :key key
                          :left left
                          :right right
                          :left-regexp (regexp-quote left)
                          :right-regexp (regexp-quote right)))))

(defun embrace-add-pair-regexp (key left-regexp right-regexp read-function)
  (assq-delete-all key embrace--pairs-list)
  (add-to-list 'embrace--pairs-list
               (cons key (make-embrace-pair-struct
                          :key key
                          :read-function read-function
                          :left-regexp left-regexp
                          :right-regexp right-regexp))))

(defun embrace--setup-defaults ()
  (dolist (pair '((?\( . ("(" . ")"))
                  (?\) . ("( " . " )"))
                  (?\{ . ("{" . "}"))
                  (?\} . ("{ " . " }"))
                  (?\[ . ("[" . "]"))
                  (?\] . ("[ " . " ]"))
                  (?> . ("<" . ">"))
                  (?\" . ("\"" . "\""))
                  (?\' . ("\'" . "\'"))))
    (embrace-add-pair (car pair) (cadr pair) (cddr pair)))
  (embrace-add-pair-regexp ?t "u]*?>" "u]*?>" 'embrace-with-tag)
  (embrace-add-pair-regexp ?f "\\(\\w\\|\\s_\\)+?(" ")" 'embrace-with-function))

(embrace--setup-defaults)
(make-variable-buffer-local 'embrace--pairs-list)

(defun embrace-with-tag ()
  (let* ((input (read-string "Tag: "))
         (_ (string-match "\\([0-9a-z-]+\\)\\(.*?\\)[>]*$" input))
         (tag  (match-string 1 input))
         (rest (match-string 2 input)))
    (cons (format "<%s%s>" (or tag "") (or rest ""))
          (format "</%s>" (or tag "")))))

(defun embrace-with-function ()
  (let ((fname (read-string "Function: ")))
    (cons (format "%s(" (or fname "")) ")")))

(defun embrace--get-region-overlay (open close)
  (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
    (let ((expand-region-fast-keys-enabled nil))
      (save-excursion
        (when (looking-at open)
          (er/expand-region 1))
        (while (and (not (= (point) (point-min)))
                    (not (and (looking-at open)
                              (save-excursion
                                (goto-char (region-end))
                                (looking-back close)))))
          (er/expand-region 1))
        (when (not (and (looking-at open)
                        (save-excursion
                          (goto-char (region-end))
                          (looking-back close))))
          (setq mark-active nil))
        (when (use-region-p)
          (make-overlay (region-beginning) (region-end) nil nil t))))))

(defun embrace--insert (char overlay)
  (let* ((struct (assoc-default char embrace--pairs-list))
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
          (goto-char (overlay-end overlay))
          (insert close))
      (delete-overlay overlay))))

(defun embrace--delete (char &optional change-p)
  (let* ((struct (assoc-default char embrace--pairs-list))
         (open (embrace-pair-struct-left-regexp struct))
         (close (embrace-pair-struct-right-regexp struct))
         (overlay (embrace--get-region-overlay open close)))
    (unless overlay
      (error "No such a pair found"))
    (unwind-protect
        (progn
          (save-excursion
            (goto-char (overlay-start overlay))
            (when (looking-at open)
              (delete-char (string-width (match-string 0))))
            (goto-char (overlay-end overlay))
            (when (looking-back close)
              (backward-delete-char (string-width (match-string 0)))))
          (when change-p overlay))
      (unless change-p (delete-overlay overlay)))))

(defun embrace--internal (change-p)
  (let* ((char (read-char "Delete pair: "))
         (overlay (embrace--delete char change-p)))
    (and change-p
         (overlayp overlay)
         (embrace--insert (read-char "Insert pair: ") overlay))))

;;;###autoload
(defun embrace-delete ()
  (interactive)
  (embrace--internal nil))

;;;###autoload
(defun embrace-change ()
  (interactive)
  (embrace--internal t))

;;;###autoload
(defun embrace-add ()
  (interactive)
  (let (mark-func
        overlay)
    (save-excursion
      ;; only ask for semantic unit if region isn't already set
      (unless (use-region-p)
        (setq mark-func (assoc-default (read-char "Semantic unit: ")
                                       embrace-semantic-units-alist))
        (unless (fboundp mark-func)
          (error "No such a semantic unit"))
        (funcall mark-func))
      (setq overlay (make-overlay (region-beginning) (region-end) nil nil t))
      (embrace--insert (read-char "Add pair: ") overlay)
      (delete-overlay overlay))))

;;;###autoload
(defun embrace-commander ()
  (interactive)
  (let ((char (read-char "Command [acd]: ")))
    (cond
     ((eq char ?a)
      (call-interactively 'embrace-add))
     ((eq char ?c)
      (call-interactively 'embrace-change))
     ((eq char ?d)
      (call-interactively 'embrace-delete))
     (t
      (error "Unknow command")))))

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

;;;###autoload
(defun embrace-org-mode-hook ()
  (dolist (lst '((?= "=" . "=")
                 (?~ "~" . "~")
                 (?/ "/" . "/")
                 (?* "*" . "*")
                 (?_ "_" . "_")
                 (?+ "+" . "+")
                 (?k "@@html:<kbd>@@" . "@@html:</kbd>@@")))
    (embrace-add-pair (car lst) (cadr lst) (cddr lst))))

(provide 'embrace)
;;; embrace.el ends here
