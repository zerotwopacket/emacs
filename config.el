(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(setq package-list
      '(evil general ivy counsel ivy-rich
        org-bullets projectile which-key
        magit))

(dolist (package package-list)
  (unless (package-installed-p package)
  (package-install package)))

;; Enable Evil
(require 'evil)
(evil-mode 1)

(column-number-mode)
(global-display-line-numbers-mode 1)
(load-theme 'modus-vivendi t)
(setq visible-bell 1);

(set-frame-font "Mononoki Nerd Font Mono 12" nil t)

(require 'general)
(use-package general
  :config
  (general-evil-setup)

  (general-create-definer ztp/leader-keys
  :states '(normal insert visual emacs)
  :keymaps 'override
  :prefix "SPC" ;; set leader
  :global-prefix "M-SPC") ;; access leader in insert mode

  (ztp/leader-keys
      "b" '(:ignore t :wk "buffer")
      "b b" '(switch-to-buffer :wk "Switch buffer")
      "b k" '(kill-this-buffer :wk "Kill this buffer")
      "b n" '(next-buffer :wk "Next buffer")
      "b p" '(previous-buffer :wk "Previous buffer")
      "b r" '(revert-buffer :wk "Reload buffer"))

  (ztp/leader-keys
      "p" '(projectile-command-map :wk "Projectile"))

  (ztp/leader-keys
      "g" '(:ignore t :wk "Git")    
      "g /" '(magit-displatch :wk "Magit dispatch")
      "g ." '(magit-file-displatch :wk "Magit file dispatch")
      "g b" '(magit-branch-checkout :wk "Switch branch")
      "g c" '(:ignore t :wk "Create") 
      "g c b" '(magit-branch-and-checkout :wk "Create branch and checkout")
      "g c c" '(magit-commit-create :wk "Create commit")
      "g c f" '(magit-commit-fixup :wk "Create fixup commit")
      "g C" '(magit-clone :wk "Clone repo")
      "g f" '(:ignore t :wk "Find") 
      "g f c" '(magit-show-commit :wk "Show commit")
      "g f f" '(magit-find-file :wk "Magit find file")
      "g f g" '(magit-find-git-config-file :wk "Find gitconfig file")
      "g F" '(magit-fetch :wk "Git fetch")
      "g g" '(magit-status :wk "Magit status")
      "g i" '(magit-init :wk "Initialize git repo")
      "g l" '(magit-log-buffer-file :wk "Magit buffer log")
      "g r" '(vc-revert :wk "Git revert file")
      "g s" '(magit-stage-file :wk "Git stage file")
      "g t" '(git-timemachine :wk "Git time machine")
      "g u" '(magit-stage-file :wk "Git unstage file"))

)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

(require 'ivy-rich)
(ivy-rich-mode 1)
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(use-package projectile
  :config
  (projectile-mode +1))

(use-package which-key
  :init
    (which-key-mode 1)
  :diminish
  :config
  (setq which-key-side-window-location 'bottom
	  which-key-sort-order #'which-key-key-order-alpha
	  which-key-allow-imprecise-window-fit nil
	  which-key-sort-uppercase-first nil
	  which-key-add-column-padding 1
	  which-key-max-display-columns nil
	  which-key-min-display-lines 6
	  which-key-side-window-slot -10
	  which-key-side-window-max-height 0.25
	  which-key-idle-delay 0.8
	  which-key-max-description-length 25
	  which-key-allow-imprecise-window-fit nil
	  which-key-separator " → " ))

(use-package magit)
