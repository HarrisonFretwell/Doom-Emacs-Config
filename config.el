;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Harrison Fretwell"
      user-mail-address "hey@harrisonfretwell.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "MonoLisa Nerd Font" :size 14 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "Alegreya SC" :size 15)
      )
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-spacegrey)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Set doom window size
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; (setq initial-frame-alist '((top . 1) (left . 1) (width . 143) (height . 55)))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/Org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Autosave and backup
(setq auto-save-default t
      make-backup-files t)
;;Disable exit warning
(setq confirm-kill-emacs nil)
;;Maximise window on startup
;;(add-to-list 'initial-frame-alist '(maximized))
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;Set line spacing
(setq line-spacing 3)

;; Allow hash to be entered
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

;; accept completion from copilot and fallback to company

;; (use-package! copilot
;;   :hook (prog-mode . copilot-mode)
;;   :bind (("C-f" . 'copilot-accept-completion)))


;; (setq-hook! 'typescript-tsx-mode-hook +format-with-lsp nil)
;; (setq-hook! 'typescript-mode-hook +format-with-lsp nil)
;; (setq-hook! 'js-mode-hook +format-with-lsp nil)
;; Disable LSP's formatter and use `format-all' instead because it deletes code otherwise
(setq +format-with-lsp nil)
(setq lsp-idle-delay 0.200)
;; Give access to more memory
(setq read-process-output-max (* 1024 1024)) ;; 1mb
;; Increase error line width
(after! lsp-ui
  (setq lsp-ui-sideline-diagnostic-max-lines 2))
;; (setq lsp-completion-provider t)
;; (defun baal-setup-lsp-company ()
;;   (setq-local company-backends
;;               '(company-capf company-dabbrev company-dabbrev-code)))



;; (after! company
;;   (setq company-minimum-prefix-length 2
;;         company-idle-delay 0.35 ;; How long to wait before popping up
;;         company-tooltip-limit 15 ;; Limit on how many options to display
;;         company-tooltip-align-annotations t ;; Align annotations to the right
;;         company-selection-wrap-around t ;; Wrap around to beginning when you hit bottom of suggestions
;;         company-dabbrev-downcase t ;; Don't automatically downcase completions
;;         ))

(use-package! prettier
  :hook ((tsx-mode . prettier-mode)
         (typescript-mode . prettier-mode)
         (js-mode . prettier-mode)
         (css-mode . prettier-mode)))




(use-package! evil-snipe
  :config (setq evil-snipe-scope 'buffer
                evil-snipe-repeat-scope 'visible
                evil-snipe-enable-highlight t
                evil-snipe-enable-incremental-highlight t
                evil-snipe-enable-key-bindings t))

;; Disable certain ligatures
(plist-put! +ligatures-extra-symbols
            :true          nil
            :false         nil
            )
(let ((ligatures-to-disable '(:true :false)))
  (dolist (sym ligatures-to-disable)
    (plist-put! +ligatures-extra-symbols sym )))


;; DOn't think this is working
(after! tree-sitter
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))

;; Org journal
(setq org-journal-date-prefix "#+TITLE: "
      org-journal-time-prefix "* "
      org-journal-date-format "%a, %Y-%m-%d"
      org-journal-file-format "%Y-%m-%d.org")

;; === Git setup ===

;;GPG path setup
(setq auth-sources '("~/.authinfo.gpg"))
(require 'epa-file)
(custom-set-variables '(epg-gpg-program  "/opt/homebrew/opt/gnupg@2.2/bin/gpg"))
(epa-file-enable)


(setq deft-directory "~/Documents/Org")

;; @see https://bitbucket.org/lyro/evil/issue/511/let-certain-minor-modes-key-bindings
(with-eval-after-load 'git-timemachine
  (evil-make-overriding-map git-timemachine-mode-map 'normal)
  ;; force update evil keymaps after git-timemachine-mode loaded
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))


;; Magit settings
(setq magit-list-refs-sortby "-creatordate")



;; Tsx mode
;;(require 'origami)
;; (require 'tsx-mode)
;;   (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-mode))


;; Godot
(setq gdscript-godot-executable "/System/Volumes/Data/Applications/Godot.app")


;; Obsidian
(require 'obsidian)
(obsidian-specify-path "/Users/harrisonfretwell/Library/Mobile Documents/iCloud~md~obsidian/Documents/Obsidian")
;; If you want a different directory of `obsidian-capture':
(setq obsidian-inbox-directory "Inbox")

;; Replace standard command with Obsidian.el's in obsidian vault:
(bind-key (kbd "C-c C-o") 'obsidian-follow-link-at-point 'obsidian-mode-map)

;; Use either `obsidian-insert-wikilink' or `obsidian-insert-link':
(bind-key (kbd "C-c C-l") 'obsidian-insert-wikilink 'obsidian-mode-map)

;; Optionally you can also bind `obsidian-jump' and `obsidian-capture'
;; replace "YOUR_BINDING" with the key of your choice:
;; (bind-key (kbd "YOUR_BINDING") 'obsidian-jump)
;; (bind-key (kbd "YOUR_BINDING") 'obsidian-capture)

;; Activate detectino of Obsidian vault
(global-obsidian-mode t)



;; == HARPOON ==
;; You can use this hydra menu that have all the commands
(map! :n "C-SPC" 'harpoon-quick-menu-hydra)
(map! :n "C-s" 'harpoon-add-file)

;; And the vanilla commands
(map! :leader "j c" 'harpoon-clear)
(map! :leader "j f" 'harpoon-toggle-file)
(map! :leader "1" 'harpoon-go-to-1)
(map! :leader "2" 'harpoon-go-to-2)
(map! :leader "3" 'harpoon-go-to-3)
(map! :leader "4" 'harpoon-go-to-4)
(map! :leader "5" 'harpoon-go-to-5)
(map! :leader "6" 'harpoon-go-to-6)
(map! :leader "7" 'harpoon-go-to-7)
(map! :leader "8" 'harpoon-go-to-8)
(map! :leader "9" 'harpoon-go-to-9)

;; == Rust ==
(after! lsp-rust
  (setq lsp-rust-server 'rust-analyzer))
