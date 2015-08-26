(autoload 'no-word "no-word" "word to txt")
(add-to-list 'auto-mode-alist '("\\.doc\\'" . no-word))
(require 'zlc)
(require 'emms)
(require 'emms-setup)
(emms-devel)
(emms-default-players)
(require 'emms-browser)
(setq emms-source-file-default-directory "~/Desktop/ms")
(global-set-key (kbd "ESC M-r") 'emms-playlist-mode-go)
(global-set-key (kbd "ESC M-m") 'emms-add-playlist)
(global-set-key (kbd "ESC M-p") 'emms-pause)
;(require 'sunrise-commander)
(add-to-list 'load-path "/usr/share/emacs/site-lisp/nav")
(require 'nav)
(nav-disable-overeager-window-splitting)
(global-set-key [f2] 'nav-toggle)
(global-set-key [f1] 'dired)
(global-set-key [f5] 'speedbar)
(require 'winner)
(winner-mode 1)
(iswitchb-mode 1)
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(global-linum-mode 1)
(setq-default c-basic-offset 4)
(menu-bar-mode -1)
(global-auto-revert-mode t)
(tool-bar-mode -1)
(windmove-default-keybindings 'meta)

(global-set-key [f9] 'compile)
(global-set-key [f8] 'shell-command)
(global-set-key "\C-t" 'shell)
(global-set-key [f4] 'revert-buffer)
(require 'w3m-load)
(setq browse-url-browser-function 'w3m-browse-url)
(setq w3m-use-cookies t)
;(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)

(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
(add-to-list 'load-path "~/.emacs.d/")
(autoload 'color-theme-molokai "color-theme-molokai.elc" "Molokai theme." t)
;(require 'color-theme-molokai)
(color-theme-molokai)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-minor-mode c-helpers-minor-mode
  "This mode contains little helpers for C developement"
  nil
  ""
  '(((kbd "{") . insert-c-block-parentheses))
)

(defun insert-c-block-parentheses ()
  (interactive)
  (insert "{")
  (newline)
  (newline)
  (insert "}")
  (indent-for-tab-command)
  (previous-line)
  (indent-for-tab-command)
  )

(global-set-key (kbd "ESC M-[") 'c-helpers-minor-mode)
(require 'yasnippet)
(yas--initialize)

;(setq yas/prompt-functions '( yas/dropdown-prompt yas/x-prompt  yas/ido-prompt yas/completing-prompt))

(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "/usr/share/auto-complete/ac-dict")
(add-to-list 'ac-dictionary-directories "/usr/share/emacs/site-lisp/auto-complete/ac-dict")
(require 'auto-complete-clang)
(setq clang-completion-suppress-error 't)
(setq ac-auto-start nil)
(ac-set-trigger-key "TAB")
(global-set-key (kbd "M-TAB") 'auto-complete)
;(global-set-key [f9] 'gdb)
;(global-set-ket [f8])
;(setq ac-expand-on-auto-complete nil)
;(ac-config-default)
;(setq ac-auto-show-menu 2)
(define-key ac-completing-map "\M-/" 'ac-stop)
(global-auto-complete-mode 1)
;(global-set-key [f5] 'dired)

(setq ac-clang-flags 
      (mapcar (lambda (item)(concat "-I" item))
                    '("/usr/lib/gcc/i686-pc-linux-gnu/4.7.2/include/"
              "/usr/include/c++/4.7.2/i686-pc-linux-gnu/"
              "/usr/include/" 
              "/usr/include/c++/4.7.2"
              )))
;(setq ac-sources '(ac-source-words-in-same-mode-buffers))
(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang 
;                   ac-source-yasnippet
                     ac-source-words-in-same-mode-buffers
                     ac-source-files-in-current-dir) ac-sources)))
(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
(add-hook 'c-mode-common-hook 'c-helpers-minor-mode)
;(add-hook 'c-mode-common-hook 'flymake-mode)
(global-set-key (kbd "ESC M-e") 'flymake-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist (append '(("/*.\.py$" . python-mode)) auto-mode-alist))

(require 'lambda-mode)
(add-hook 'python-mode-hook #'lambda-mode 1)
(setq lambda-symbol (string (make-char 'greek-iso8859-7 107)))
;(require 'jedi)
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 
    (lambda ()
        (setq jedi:server-command 
            (list "/usr/bin/python" jedi:server-script))))
(add-hook 'python-mode-hook 'jedi:setup)
;(require 'ipython)
;(require 'anything) (require 'anything-ipython)
;(when (require 'anything-show-completion nil t)
;   (use-anything-show-completion 'anything-ipython-complete
;       '(length initial-pattern)))
;(add-hook 'python-mode-hook 'ac-config-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist (append '(("/*.\.java$" . java-mode)) auto-mode-alist))

(defun java-binds() 
  (lambda () (global-set-key (kbd "ESC M-i") 'ajc-import-class-under-point))
  )

(require 'ajc-java-complete-config)
(add-hook 'java-mode-hook 'ajc-java-complete-mode)
(add-hook 'java-mode-hook 'java-binds)
;(add-hook 'find-file-hook 'ajc-4-jsp-find-file-hook)
;(require 'javadoc-lookup)
;(javadoc-lookup "/usr/share/doc/openjdk-6-jdk/api")

;;;;;;;;;;;;;;;;;;;;;;;;;
;(require 'haskell-mode)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;(add-hook 'haskell-mode-hook 'haskell-font-lock-symbols t)
;(set 'haskell-font-lock-symbols t)

;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-startup-message t)
(put 'downcase-region 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p) 


(defun win-resize-top-or-bot () "Figure out if the current window is on top, bottom or in the middle" 
  (let* ((win-edges (window-edges)) (this-window-y-min (nth 1 win-edges)) 
     (this-window-y-max (nth 3 win-edges)) (fr-height (frame-height))) 
    (cond ((eq 0 this-window-y-min) "top") ((eq (- fr-height 1) this-window-y-max) "bot") (t "mid"))))
(defun win-resize-left-or-right () "Figure out if the current window is to the left, right or in the middle" (let* ((win-edges (window-edges)) (this-window-x-min (nth 0 win-edges)) (this-window-x-max (nth 2 win-edges)) (fr-width (frame-width))) (cond ((eq 0 this-window-x-min) "left") ((eq (+ fr-width 4) this-window-x-max) "right") (t "mid"))))
(defun win-resize-enlarge-horiz () (interactive) (cond ((equal "top" (win-resize-top-or-bot)) (enlarge-window -5)) ((equal "bot" (win-resize-top-or-bot)) (enlarge-window 5)) ((equal "mid" (win-resize-top-or-bot)) (enlarge-window -5)) (t (message "nil")))) 
(defun win-resize-minimize-horiz () (interactive) (cond ((equal "top" (win-resize-top-or-bot)) (enlarge-window 5)) ((equal "bot" (win-resize-top-or-bot)) (enlarge-window -5)) ((equal "mid" (win-resize-top-or-bot)) (enlarge-window 5)) (t (message "nil")))) 
(defun win-resize-enlarge-vert () (interactive) (cond ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally -5)) ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally 5)) ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally -5))))
(defun win-resize-minimize-vert () (interactive) (cond ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally 5)) (((equal ) "right" (win-resize-left-or-right)) (enlarge-window-horizontally -5)) ((equal "mid" (win-resize-left-(or  )r-right)) (enlarge-window-horizontally 5)))) 
(global-set-key (kbd "ESC M-j") 'win-resize-mi2nimize-vert) 
(global-set-key (kbd "ESC M-k") 'win-resize-enlarge-vert)
(global-set-key (kbd "ESC M-h") 'win-resize-minimize-horiz)
(global-set-key (kbd "ESC M-l") 'win-resize-enlarge-horiz)
(global-set-key (kbd "ESC M-k") 'win-resize-enlarge-horiz)
(global-set-key (kbd "ESC M-j") 'win-resize-minimize-horiz) 
(global-set-key (kbd "ESC M-h") 'win-resize-enlarge-vert)
(global-set-key (kbd "ESC M-l") 'win-resize-minimize-vert)
