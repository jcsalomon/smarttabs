;;; smart-tabs-mode.el --- Intelligently indent with tabs, align with spaces!

;; Copyright © 2011 John Croisant <jacius@gmail.com>
;; Copyright © 2011 Joel C. Salomon <joelcsalomon@gmail.com>
;; Copyright © 2012 Alan Pearce <alan@alanpearce.co.uk>
;; Copyright © 2012 Daniel Dehennin <daniel.dehennin@baby-gnu.org>

;; Author: John Croisant <jacius@gmail.com>
;;         Joel C. Salomon <joelcsalomon@gmail.com>
;;         Alan Pearce <alan@alanpearce.co.uk>
;;         Daniel Dehennin <daniel.dehennin@baby-gnu.org>
;; URL: http://www.emacswiki.org/emacs/SmartTabs
;; Created: 19 Sep 2011
;; Version: 0.2
;; Keywords: languages

;; This file is not part of GNU Emacs.

;;; License:

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provide a semantic way of using tab characters in
;; source code: tabs for indentation, spaces for alignment.

;;; Installation:

;; To use, save smart-tabs-mode.el to a a directory on your load-path
;; (e.g., ~/.emacs.d/elisp), then add the following to your .emacs file:
;;
;;  (autoload 'smart-tabs-mode "smart-tabs-mode"
;;    "Intelligently indent with tabs, align with spaces!")
;;  (autoload 'smart-tabs-mode-enable "smart-tabs-mode")
;;  (autoload 'smart-tabs-advice "smart-tabs-mode")
;;
;; Then, for each language you want to use smart tabs, set up a hook
;; and advice like so:
;;
;;  (add-hook 'MODE-HOOK 'smart-tabs-mode-enable)
;;  (smart-tabs-advice INDENT-FUNC TAB-WIDTH-VAR)
;;
;; Note that it might be preferable to delay calling smart-tabs-advice
;; until after the major mode is loaded and evaluated:
;;
;; (eval-after-load 'MODE-FEATURE
;;   '(smart-tabs-advice INDENT-FUNC TAB-WIDTH-VAR))
;;
;; Or:
;;
;;  (add-hook 'MODE-HOOK (lambda ()
;;                         (smart-tabs-mode-enable)
;;                         (smart-tabs-advice INDENT-FUNC TAB-WIDTH-VAR)))
;;
;; Here are some specific examples for a few popular languages which
;; can be enabled by 'smart-tab-insinuate':
;;
;;  ;; Load all the following in one pass
;;  (smart-tabs-insinuate 'c 'javascript 'cperl 'python 'ruby)
;;
;;  ;; C/C++
;;  (add-hook 'c-mode-hook 'smart-tabs-mode-enable)
;;  (smart-tabs-advice c-indent-line c-basic-offset)
;;  (smart-tabs-advice c-indent-region c-basic-offset)
;;
;;  ;; JavaScript
;;  (add-hook 'js2-mode-hook 'smart-tabs-mode-enable)
;;  (smart-tabs-advice js2-indent-line js2-basic-offset)
;;
;;  ;; Perl (cperl-mode)
;;  (add-hook 'cperl-mode-hook 'smart-tabs-mode-enable)
;;  (smart-tabs-advice cperl-indent-line cperl-indent-level)
;;
;;  ;; Python
;;  (add-hook 'python-mode-hook 'smart-tabs-mode-enable)
;;  (smart-tabs-advice python-indent-line-1 python-indent)
;;
;;  ;; Ruby
;;  (add-hook 'ruby-mode-hook 'smart-tabs-mode-enable)
;;  (smart-tabs-advice ruby-indent-line ruby-indent-level)
;;
;; This package is derived from <http://www.emacswiki.org/emacs/SmartTabs>
;; as modified by John Croisant (jacius), along with Julien Fontanet and
;; Tomita Hiroshi (tomykaira).
;;
;; Modification history is at <https://github.com/jcsalomon/smarttabs>.

;;; Code:

(require 'advice)

(defvar smart-tabs-mode nil
  "Define if smart-tabs-mode is enabled")

(defvar smart-tabs-insinuate-alist
  '((c . (lambda ()
           (add-hook 'c-mode-hook
                     (lambda ()
                       (smart-tabs-mode-enable)
                       (smart-tabs-advice c-indent-line c-basic-offset)
                       (smart-tabs-advice c-indent-region c-basic-offset)))))
    (javascript . (lambda ()
                    (add-hook 'js2-mode-hook
                              (lambda ()
                                (smart-tabs-mode-enable)
                                (smart-tabs-advice js2-indent-line js2-basic-offset)))))
    (cperl . (lambda ()
               (add-hook 'cperl-mode-hook
                         (lambda ()
                           (smart-tabs-mode-enable)
                           (smart-tabs-advice cperl-indent-line cperl-indent-level)))))
    (python . (lambda ()
                (add-hook 'python-mode-hook
                          (lambda ()
                            (smart-tabs-mode-enable)
                            (smart-tabs-advice python-indent-line-1 python-indent)
                            (if (featurep 'python-mode)
                                (progn
                                  (smart-tabs-advice py-indent-line py-indent-offset)
                                  (smart-tabs-advice py-newline-and-indent py-indent-offset)
                                  (smart-tabs-advice py-indent-region py-indent-offset)))))))
    (ruby . (lambda ()
              (add-hook 'ruby-mode-hook
                        (lambda ()
                          (smart-tabs-mode-enable)
                          (smart-tabs-advice ruby-indent-line ruby-indent-level)))))
    (nxml . (lambda ()
              (add-hook 'nxml-mode-hook
                        (lambda ()
                          (smart-tabs-mode-enable)
                          (smart-tabs-advice nxml-indent-line nxml-child-indent)))))
    )
  "Alist of language name and their activation code.
Smarttabs is enabled in mode hook.")

(defmacro smart-tabs-mode/no-tabs-mode-advice (function)
  `(unless (ad-find-advice ',function 'around 'smart-tabs)
     (defadvice ,function (around smart-tabs activate)
       (if smart-tabs-mode
           (let ((indent-tabs-mode nil)) ad-do-it)
         ad-do-it))))

;;;###autoload
(define-minor-mode smart-tabs-mode
  "Intelligently indent with tabs, align with spaces!"

  (progn
    (smart-tabs-mode/no-tabs-mode-advice align)
    (smart-tabs-mode/no-tabs-mode-advice align-regexp)
    (smart-tabs-mode/no-tabs-mode-advice indent-relative)

    (unless
        (ad-find-advice 'indent-according-to-mode 'around 'smart-tabs)
      (defadvice indent-according-to-mode (around smart-tabs activate)
        (if smart-tabs-mode
            (let ((indent-tabs-mode indent-tabs-mode))
              (if (memq indent-line-function
                        '(indent-relative
                          indent-relative-maybe))
                  (setq indent-tabs-mode nil))
              ad-do-it)
          ad-do-it)))
    ))

;;;###autoload
(defun smart-tabs-mode-enable ()
  "Enable smart-tabs-mode."
  (smart-tabs-mode t))

;;;###autoload
(defmacro smart-tabs-advice (function offset)
  `(progn
     (defvaralias ',offset 'tab-width)
     (defadvice ,function (around smart-tabs activate)
       (cond
        (smart-tabs-mode
         (save-excursion
           (beginning-of-line)
           (while (looking-at "\t*\\( +\\)\t+")
             (replace-match "" nil nil nil 1)))
         (setq tab-width tab-width)
         (let ((indent-tabs-mode t)
               (tab-width fill-column)
               (,offset fill-column))
           (unwind-protect
               (progn ad-do-it))))
        (t
         ad-do-it)))))

;;;###autoload
(defun smart-tabs-insinuate (&rest languages)
  "Enable smart-tabs-mode for LANGUAGES.
LANGUAGES is a list of SYMBOL or NAME as defined in
'smart-tabs-insinuate-alist' alist or a language using standard named
indent function and indent level.
"
  (mapc (lambda (lang)
           (let ((lang-map (assoc lang smart-tabs-insinuate-alist))
                 (lang-param (smart-tabs-get-standard-language lang)))
             (cond ((and (null lang-map)
                         (not (null (car lang-param)))
                         (not (null (nth 1 lang-param)))
                         (not (null (nth 2 lang-param))))
                    (smart-tabs-guess-insinuate lang-param))
                   ((null lang-map) (error (format "Unknown smart-tab-mode capable language '%s'" lang)))
                   (t (funcall (cdr lang-map))))))
        languages))

(defun smart-tabs-guess-insinuate (lang-param)
  "Enable smart-tabs-mode if language respect standard naming.
Several languages define a '<LANGUAGE>-indent-line' function and
'<LANGUAGE>-indent-level' variable to control indentation.
LANG-PARAM is a list of HOOK INDENT-FUNCTION INDENT-LEVEL, if
thoses are defined, we use them."
  (let ((lang-hook (car lang-param))
        (indent-function (nth 1 lang-param))
        (indent-level (nth 2 lang-param)))
  (if (and (not (null lang-hook))
           (and (not (null indent-function))
                (fboundp indent-function))
           (and (not (null indent-level))
                (boundp indent-level)))
      (add-hook lang-hook
                `(lambda ()
                    (smart-tabs-mode-enable)
                    (smart-tabs-advice ,indent-function ,indent-level))))))

(defun smart-tabs-get-standard-language (language)
  "Return a list of HOOK INDENT-FUNCTION INDENT-LEVEL for a language."
  (let ((indent-function (intern-soft (concat (symbol-name language) "-indent-line")))
        (indent-level (intern-soft (concat (symbol-name language) "-indent-level")))
        (lang-hook (intern-soft (concat (symbol-name language) "-mode-hook"))))
    (list lang-hook indent-function indent-level)))

(provide 'smart-tabs-mode)

;;; smart-tabs-mode.el ends here
