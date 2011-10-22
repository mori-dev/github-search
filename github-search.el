;;; github-search.el

;; Copyright (C) 2011-2015 mori_dev

;; Author: mori_dev <mori.dev.asdf@gmail.com>
;; Keywords: github, search
;; Prefix: gs:

;; Most of code taken from google.el (https://github.com/mcfunley/dotemacs/blob/master/google.el)

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

;;; Setting Sample

;; (require 'github-search)

;; (defalias 'g 'gs:code-search)
;; (defalias 'ga 'gs:all-search)
;; (defalias 'gu 'gs:user-search)
;; (defalias 'gr 'gs:repositories-search)


(eval-when-compile
  (require 'cl))

(defvar gs:major-mode-lang-alist
  '((c-mode                . "C")
    (c++-mode              . "C++")
    (css-mode              . "CSS")
    (html-mode             . "HTML")
    (java-mode             . "Java")
    (python-mode           . "Python")
    (perl-mode             . "Perl")
    (javascript-mode       . "JavaScript")
    (js2-mode              . "JavaScript")
    (coffee-mode           . "CoffeeScript")
    (haskell-mode          . "Haskell")
    (literate-haskell-mode . "Literate Haskell")
    (jde-mode              . "Java")
    (php-mode              . "PHP")
    (ruby-mode             . "Ruby")
    (haml-mode             . "Haml")
    (sass-mode             . "Sass")
    (feature-mode          . "Cucumber")
    (lisp-mode             . "Common Lisp")
    (emacs-lisp-mode       . "Emacs Lisp")
    (sh-mode               . "Shell")
    (yaml-mode             . "YAML")
    (diff-mode             . "Diff")))


(defun* gs:code-search (term lang)
  (interactive (list
                (gs:read-term)
                (gs:read-lang)))
  (gs:browse-url (concat "https://github.com/search?type=Code&q=" term lang)))


(defun* gs:read-lang ()
  (let* ((inp (completing-read "Language: "
                               (mapcar 'cdr gs:major-mode-lang-alist)
                               nil t
                               (gs:get-lang)))
         (lang (replace-regexp-in-string "\s" "+" inp)))
    (if (> (length lang) 0)
        (concat "&language=" lang)
      "")))

(defun* gs:read-term ()
  (read-from-minibuffer "Term: " (current-word)))

(defun* gs:url-escape (s)
  (replace-regexp-in-string "\s" "+" s))

(defun* gs:browse-url (url)
  (browse-url (gs:url-escape url)))

(defun* gs:get-lang ()
  (cdr (assoc major-mode gs:major-mode-lang-alist)))

(defun* gs:all-search (term)
  (interactive (list (gs:read-term)))
  (let ((url (concat "https://github.com/search?q=" term)))
    (gs:browse-url url)))

(defun* gs:user-search (term)
  (interactive (list (gs:read-term)))
  (let ((url (concat "https://github.com/search?type=Users&q=" term)))
    (gs:browse-url url)))

(defun* gs:repositories-search (term)
  (interactive (list (gs:read-term)))
  (let ((url (concat "https://github.com/search?type=Repositories&q=" term)))
    (gs:browse-url url)))

(provide 'github-search)