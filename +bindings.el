;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

;(load! myhydra)

(general-define-key
 :states '(normal visual insert emacs)
 :keymaps 'override
 :prefix "SPC"
 :non-normal-prefix "M-m"
 "'" '(iterm-focus :which-key "iterm")
 "?" '(iterm-goto-filedir-or-home :which-key "iterm - goto dir")
 "/" '(counsel-rg :wich-key "rg")
 "TAB" '(switch-to-previous-buffer :which-key "prev buffer")
 "SPC" '(counsel-M-x :which-key "M-x")
 "k" '(evil-avy-goto-char-2 :which-key "jump char 2")
 "q" '(persp-switch-to-buffer :which-key "Switch workspace buffer")
 "Q" '(switch-to-buffer :which-key "Switch to buffer")
 "d" '(counsel-git-grep :which-key "git grep")
 "RET" '(bookmark-jump :which-key "Jump to bookmark")

 "a" '(:ignore t :which-key "applications")
 "ar" '(align-regexp :which-key "align-regexp")
 "at" '(+term/open :which-key "+term/open")
 "aj" '(org-journal-new-entry :which-key "new org journal entry")

 "b" '(:ignore t :which-key "buffers")
 "bb" '(persp-switch-to-buffer :which-key "Switch workspace buffer")
 "bB" '(switch-to-buffer :which-key "Switch to buffer")
 "br" '(rename-buffer :which-key "rename buffer")
 "bk" '(doom/kill-this-buffer-in-all-windows :which-key "kill buffer")
 "bs" '(doom/open-scratch-buffer :which-key "open scratch")
 "bt" '(me/switch-to-workspace-term :which-key "open project terminal")
 "bm" '(me/open-message-buffer :which-key "open message buffer")

 "e" '(:ignore t :which-key "Errors/Emacs")
 "ee" '(+default/find-in-config :which-key "find-in-my-private-config")
 "eE" '(+default/browse-config :which-key "browse my private config")
 "ed" '(+default/find-in-emacsd :which-key "find-in-emacsd")
 "eD" '(+default/browse-emacsd :which-key "browse emacsd")
 "el" '(flycheck-list-errors :which-key "List errors")
 "en" '(next-error :which-key "next errors")
 "ep" '(previous-error :which-key "next errors")

 "f" '(:ignore t :which-key "Files/Fold")
 "fd" '(+evil:delete-this-file :which-key "delete this file")
 "fe" '(me/open-module-init :which-key "open config.el for my module")
 "ff" '(counsel-find-file :which-key "find file")
 "fj" '(dired-jump :which-key "dired jump")
 ;; TODO: add fn for just copy the filename
 "fp" '(copy-file-name-to-clipboard :which-key "yank file full path")
 "fo" '(hydra-folding/body :which-key "hydra folding")
 "fr" '(counsel-recentf :which-key "recent file")

 "g" '(:ignore t :which-key "Git")
 "gs" '(magit-status :which-key "Git status")
 "ga" '(magit-stage-file :which-key "stage this file")
 "gb" '(magit-blame :which-key "Git blame")
 "gc" '(magit-commit :which-key "Git commit")
 "gd" '(magit-diff-buffer-file :which-key "Git diff")
 ;; list commits affect current function
 "gf" '(magit-log-trace-definition :which-key "show commits for this function")
 ;; list commits affect current file
 "gl" '(magit-log-buffer-file :which-key "show commits for this file")
 "gg" '(my-goto-git-gutter :which-key "Git gutter")
 "gG" '(hydra-git/body :which-key "Git gutter hydra")
 "gp" '(magit-push-current :which-key "Git push")
 "gt" '(my-git-timemachine :which-key "Git time machine")

 "h" '(:ignore t :which-key "Help/Highlight")
 "hh" '(helpful-at-point :which-key "helpful-at-point")
 "hm" '(describe-mode :which-key "Describe mode")
 "hf" '(helpful-function :which-key "Describe function")
 "hk" '(helpful-key :which-key "Describe key")
 "hv" '(helpful-variable :which-key "Describe variable")
 "hL" '(hl-highlight-thingatpt-global :which-key "highlight global")
 "hl" '(hl-highlight-thingatpt-local :which-key "highlight local")
 "hu" '(hl-unhighlight-all-local :which-key "un highlight local")
 "hU" '(hl-unhighlight-all-global :which-key "un highlight global")

 "j" '(:ignore t :which-key "Jump")
 "jd" '(dumb-jump-go :which-key "dumb-jump-go")
 "ji" '(imenu :which-key "Imenu")
 "jb" '(avy-pop-mark :which-key "jump back")
 "jI" '(imenu-anywhere :which-key "Imenu across buffers")
 "jm" '(evil-show-marks :which-key "show marks")
 "jr" '(counsel-evil-registers :which-key "show registers")
 "jo" '(+lookup/online :which-key "online search")
 "js" '(+lookup/online-select :which-key "Online providers")
 "jt" '(counsel-etags-find-tag-at-point :which-key "counsel etags")

 "l" '(:ignore t :which-key "workspace/layout")
 "ln" '(+workspace/me/new :which-key "New workspace")
 "ld" '(+workspace/delete :which-key "delete workspace")
 "ll" '(+workspace/switch-to :which-key "switch workspace")
 "lr" '(+workspace/rename :which-key "rename workspace")
 "lt" '(me/new-workspace-term :which-key "create a term-mode buffer for workspace")
 "l TAB" '(doom/jump-to-last-workspace :which-key "toggle workspace")

 "n" '(:ignore t :which-key "Notes")
 "nn" '(me/find-in-notes :which-key "find-in-notes")
 "nN" '(me/browse-notes :which-key "browse-notes")
 ;; "nd" '(showgood/find-in-dotfiles :which-key "showgood/find-in-dotfiles")
 ;; "nD" '(showgood/browse-dotfiles :which-key "showgood/browse-in-dotfiles")

 "o" '(:ignore t :which-key "bookmark")
 "om" '(bookmark-set :which-key "set bookmark")
 "ol" '(bookmark-bmenu-list :which-key "open bookmark buffer")
 "ou" '(bmkp-url-target-set :which-key "set url bookmark")
 "os" '(bmkp-set-snippet-bookmark :which-key "set snippet bookmark")
 "od" '(bmkp-dired-jump :which-key "jump to dired bookmark")

 "p" '(:ignore t :which-key "project")
 "pp" '(projectile-switch-project :which-key "projectile-switch-project")
 "pt" '(+ivy/tasks :which-key "+ivy/tasks")
 "pf" '(counsel-projectile-find-file :which-key "counsel-projectile-find-file")
 "px" '(projectile-invalidate-cache :which-key "projectile-invalidate-cache")
 "po" '(+term/open-popup-in-project :which-key "+term/open-popup-in-project")

 "s" '(:ignore t :which-key "snippets / switch")
 "sn" '(yas-new-snippet :which-key "yas-new-snippet")
 "si" '(yas-insert-snippet :which-key "yas-insert-snippet")
 "ss" '(yas-visit-snippet-file :which-key "yas-visit-snippet-file")
 "sS" '(+default/find-in-snippets :which-key "find-in-snippets")
 "sf" '(+default/find-in-snippets :which-key "yas-new-snippet")
 "sF" '(+default/browse-snippets :which-key "yas-new-snippet")

 "t"  '(:ignore t :which-key "toggle")
 ;; maybe I don't need this since we have treemacs now
 ;; "td" '(dired-sidebar-toggle-sidebar :which-key "dired-sidebar-toggle-sidebar")
 ;; "tD" '(dired-sidebar-toggle-with-current-directory :which-key "dired sidebar cur directory")
 "tv" '(visual-line-mode :which-key "visual-line-mode")
 "tf" '(visual-fill-column-mode :which-key "visual-fill-column-mode")
 "ts" '(flyspell-mode :which-key "flyspell-mode")
 "tc" '(flycheck-mode :which-key "flycheck-mode")
 "tg" '(evil-goggles-mode :which-key "+evil-goggles/toggle")
 "ti" '(highlight-indentation-mode :which-key "highlight-indentation-mode")
 "tI" '(highlight-indentation-current-column-mode :which-key "highlight-indentation-current-column-mode")
 "tt" '(treemacs :which-key "treemacs")

 "v"  '(:ignore t :which-key "vimish fold")
 "vd" '(vimish-fold-delete :which-key "fold delete")
 "vD" '(vimish-fold-delete-all :which-key "fold delete all")
 "vf" '(vimish-fold :which-key "fold")
 "vn" '(vimish-fold-next-fold :which-key "next fold")
 "vp" '(vimish-fold-previous-fold :which-key "previous fold")
 ;; seems not useful
 ;; "vv" '(vimish-fold-toggle :which-key "fold toggle")
 ;; "vV" '(vimish-fold-toggle-all :which-key "fold toggle all")
 "vu" '(vimish-fold-unfold :which-key "unfold")
 "vU" '(vimish-fold-unfold-all :which-key "unfold all")

 "w"  '(:ignore t :which-key "Windows")
 "wd" '(delete-window :which-key "delete window")
 "wD" '(ace-delete-window :which-key "ace delete window")
 "wF" '(make-frame :which-key "make frame")
 "w-" '(evil-window-split :which-key "split horizontally")
 "wv" '(evil-window-vsplit :which-key "split vertically")
 "wm" '(delete-other-windows :which-key "maximize window")
 "wt" '(window-split-toggle :which-key "toggle window layout")
 "ww" '(ace-window :which-key "ace window")
 "w TAB" '(aw-flip-window :which-key "select previous window")
 "wh" '(hydra-window/body :which-key "Window Hydra")
 "ws" '(ace-swap-window :which-key "swap window")
 "w=" '(balance-windows :which-key "balance windows")

 "z" '(:ignore t :which-key "folding")
 "zt" '(origami-toggle-all-nodes :which-key "origami-toggle-all-nodes")
 "zo" '(origami-open-node :which-key "origami-open-node")
 "zc" '(origami-close-node :which-key "origami-close-node")
 "zO" '(origami-open-node-recursively :which-key "origami-open-node-recursively")
 "zC" '(origami-close-node-recursively :which-key "origami-close-node-recursively")
 "za" '(origami-open-all-nodes :which-key "origami-open-all-nodes")
 "zm" '(origami-close-all-nodes :which-key "origami-close-all-nodes")
 "zh" '(hydra-zoom/body :which-key "hydra zoom")
 )

(general-omap
  :prefix "SPC"
  "." 'evil-avy-goto-char-2
  "l" 'evil-avy-goto-line
  "e" 'evil-avy-goto-subword-0 )

(general-omap
  "s"  'evil-surround-edit
  "S"  'evil-Surround-edit
  )

(general-vmap
  "S"  'evil-surround-region
  )

(general-define-key
 :states '(normal)
 "TAB" '(origami-toggle-node :which-key "origami-toggle-node")
 "<backtab>" '(origami-toggle-all-nodes :which-key "origami-toggle-all-nodes")
 )

(general-define-key
 :states '(normal visual insert emacs)
 "C-y" '(yank :which-key "yank")
 "C-s" '(counsel-grep-or-swiper :which-key "swiper")
 "M-y" '(counsel-yank-pop :which-key "counsel yank pop")

 "C-h" '(evil-window-left :which-key "left window")
 "C-j" '(evil-window-down :which-key "down window")
 "C-k" '(evil-window-up :which-key "up window")
 "C-l" '(evil-window-right :which-key "right window")
 "M-/" '(dabbrev-expand :which-key "hippie expand")
 "C-c <left>" '(winner-undo :which-key "winner undo")
 "C-c <right>" '(winner-redo :which-key "winner redo")

 "<f2>" '(org-clock-goto :which-key "org-clock-goto")
 "<f3>" '(org-clock-in :which-key "org-clock-in")
 "<f4>" '(org-clock-out :which-key "org-clock-out")
 "<f5> a" '(org-archive-subtree :which-key "org-archive-subtree")
 "<f5> c" '(calendar :which-key "calendar")
 "<f5> r" '(org-refile :which-key "org-refile")
 "<f8> c" '(counsel-git-grep-complete-line :which-key "counsel-git-grep-complete-line")
 "<f9> r" '(rename-buffer :which-key "rename-buffer")
 "<f9> a" '(org-attach :which-key "org-attach")
 "<f10>" '(org-capture :which-key "org-capture")
 "<f11>" '(org-agenda :which-key "org-agenda")
 "<f12>" '(org-todo :which-key "org-todo")

 )

(general-define-key
 :states '(normal visual)
 ;; ga - what-cursor-position
 "ga" '(projectile-find-other-file :which-key "toggle between h/cpp")
 "gA" '(projectile-find-other-file-other-window :which-key "toggle between h/cpp")
 "gb" '(+ivy/switch-workspace-buffer :which-key "switch workspace buffer")
 "gB" '(ivy-switch-buffer :which-key "switch all buffer")
 "gc" '(evil-commentary :which-key "evil commentary")
 "gd" '(+lookup/definition :which-key "jump to definition")
 "gD" '(+lookup/references :which-key "jump to references")
 "ge" '(+eval:region :which-key "+eval:region")
 "gE" '(+eval/buffer :which-key "+eval/buffer")
 "gf" '(counsel-find-file :which-key "find file")
 "gF" '(counsel-projectile-find-file :which-key "projectile file")

 ;; gg - evil-goto-first-line
 "gh" '(dash-at-point :which-key "jump to Dash")
 "gi" '(counsel-imenu :which-key "counsel imenu")
 "gI" '(lsp-ui-imenu :which-key "lsp-ui-imenu")

 ;; gj - evil-next-visual-line
 ;; gk - evil-previous-visual-line

 ;; gl

 "gm" '(delete-other-windows :which-key "maximize current buffer")
 "gM" '(winner-undo :which-key "restore previous window layout")

 ;; gn - evil-next-match

 "go" '(save-buffer :which-key "save buffer")
 "gp" '(+evil/reselect-paste :which-key "+evil/reselect-paste")

 ;; gq - evil-fill-and-move  (re-align text to fill column width)

 "gr" '(tldr :which-key "tldr")

 "gs" '(evil-window-vsplit :which-key "split window vertically")
 "gS" '(evil-window-split :which-key "split window horizontally")

 "gt" '(doom/jump-to-last-workspace :which-key "toggle workspace")
 "gT" '(+workspace/switch-to :which-key "list all workspace to switch")

 ;; gu - evil-downcase
 ;; gU - evil-upcase
 ;; gv - evil-visual-restore
 ;; gV - evil-visual-restore
 "gw" '(ace-window :which-key "ace window")
 "gW" '(window-split-toggle :which-key "transpose two windows")

 "gx" '(evil-exchange :which-key "evil exchange")
 ;; gy - evil-commentary-yank
 ;; "gz" '(+eval:replace-region :which-key "replace region with eval result")
 "g-" '(+evil:narrow-buffer :which-key "narrow buffer")
 "g=" '(widen :which-key "widen")
)

(general-define-key
 :states '(visual)
 "v" '(er/expand-region :which-key "expand region")
 "V" '(er/contract-region :which-key "contract region")
 )

(general-define-key
 :states '(normal visual)
 :prefix ","
 "D" '(dash-at-point :which-key "dash-at-point")
 "+" '(evil-numbers/inc-at-pt :which-key "evil-numbers/inc-at-pt")
 "-" '(evil-numbers/dec-at-pt :which-key "evil-numbers/dec-at-pt")
 "c" '(counsel-git-grep-complete-line :which-key "counsel-git-grep-complete-line")
 )

(general-define-key
 :prefix ","
 :states '(normal)
 :keymaps 'c++-mode-map
 "d" '(xref-find-definitions :which-key "find definition")
 "r" '(ccls/callers :which-key "find references")
 )

(general-define-key
 :prefix ","
 :states '(normal)
 :keymaps 'nxml-mode-map
 "xp" '(nxml-where :which-key "xpath")
 )

;; another way to print json path is to
;; switch to js2-mode, then use js2-print-json-path
(general-define-key
 :prefix ","
 :states '(normal)
 :keymaps 'json-mode-map
 "xp" '(jsons-print-path :which-key "xpath")
 )

(general-define-key
 :states '(insert normal)
 :keymaps 'wgrep-mode-map
 ":" '(evil-ex :which-key "evil-ex")
 "M-x" '(counsel-M-x :which-key "M-x")
 "C-;" #'evil-normal-state
 )

;; NOTE: need to use 'override to make M-y works in evil-ex-map
(general-define-key
 :keymaps 'override
 "M-y" '(counsel-yank-pop :which-key "counsel-yank-pop")
 )

(general-define-key
 :states '(normal)
 :keymaps 'slime-mode-indirect-map
 :prefix ","
 "cc" '(slime-compile-defun :which-key "slime-compile-defun")
 )

;; this two lines are needed to make C-i works for evil-jump-forward
;; historically C-i and <Tab> has same keycode
;; https://emacs.stackexchange.com/questions/17509/how-to-distinguish-c-i-from-tab
;; https://www.reddit.com/r/emacs/comments/80yna2/evil_how_to_have_ci_behave_like_in_vim/
(define-key input-decode-map "\C-i" [C-i])
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "<C-i>") 'evil-jump-forward))

(general-define-key
  :states '(normal)
  :keymaps 'web-mode-map
  "<tab>" '(web-mode-fold-or-unfold :which-key "fold")
  "<backtab>" '(web-mode-element-children-fold-or-unfold :which-key "fold child")
  ",e" '(web-mode-element-content-select :which-key "mark element content")
)

;; expand-region's prompt can't tell what key contract-region is bound to, so we
;; tell it explicitly.
(setq expand-region-contract-fast-key "V")

(map!
    (:map* (help-mode-map helpful-mode-map)
    :n "o"  #'ace-link-help
    :n "q"  #'quit-window
    :n "Q"  #'ivy-resume
    :n "]l" #'forward-button
    :n "[l" #'backward-button)

    (:after company
        (:map company-active-map
          ;; Don't interfere with `evil-delete-backward-word' in insert mode
          "C-w"     nil
          "C-n"     #'company-select-next
          "C-p"     #'company-select-previous
          "C-j"     #'company-select-next
          "C-k"     #'company-select-previous
          "C-h"     #'company-show-doc-buffer
          "C-u"     #'company-previous-page
          "C-d"     #'company-next-page
          "C-s"     #'company-filter-candidates
          (:when (featurep! :completion helm)
            "C-S-s" #'helm-company)
          (:when (featurep! :completion ivy)
            "C-S-s" #'counsel-company)
          "C-SPC"   #'company-complete-common
          [tab]     #'company-complete-common-or-cycle
          [backtab] #'company-select-previous)
        ;; Automatically applies to `company-filter-map'
        (:map company-search-map
          "C-n"     #'company-select-next-or-abort
          "C-p"     #'company-select-previous-or-abort
          "C-j"     #'company-select-next-or-abort
          "C-k"     #'company-select-previous-or-abort
          "C-s"     (λ! (company-search-abort) (company-filter-candidates))
          [escape]  #'company-search-abort))

      ;; evil-mc
      (:prefix "gz"
        :nv "m" #'evil-mc-make-all-cursors
        :nv "u" #'evil-mc-undo-all-cursors
        :nv "z" #'+evil/mc-make-cursor-here
        :nv "t" #'+evil/mc-toggle-cursors
        :nv "n" #'evil-mc-make-and-goto-next-cursor
        :nv "p" #'evil-mc-make-and-goto-prev-cursor
        :nv "N" #'evil-mc-make-and-goto-last-cursor
        :nv "P" #'evil-mc-make-and-goto-first-cursor
        :nv "d" #'evil-mc-make-and-goto-next-match
        :nv "D" #'evil-mc-make-and-goto-prev-match
        :nv "j" #'evil-mc-make-cursor-move-next-line
        :nv "k" #'evil-mc-make-cursor-move-prev-line)
      (:after evil-mc
        :map evil-mc-key-map
        :nv "C-n" #'evil-mc-make-and-goto-next-cursor
        :nv "C-N" #'evil-mc-make-and-goto-last-cursor
        :nv "C-p" #'evil-mc-make-and-goto-prev-cursor
        :nv "C-P" #'evil-mc-make-and-goto-first-cursor)

      ;; evil-multiedit
      :v  "R"     #'evil-multiedit-match-all
      :n  "M-d"   #'evil-multiedit-match-symbol-and-next
      :n  "M-D"   #'evil-multiedit-match-symbol-and-prev
      :v  "M-d"   #'evil-multiedit-match-and-next
      :v  "M-D"   #'evil-multiedit-match-and-prev
      :nv "C-M-d" #'evil-multiedit-restore
      (:after evil-multiedit
        (:map evil-multiedit-state-map
          "M-d" #'evil-multiedit-match-and-next
          "M-D" #'evil-multiedit-match-and-prev
          "RET" #'evil-multiedit-toggle-or-restrict-region)
        (:map (evil-multiedit-state-map evil-multiedit-insert-state-map)
          "C-n" #'evil-multiedit-next
          "C-p" #'evil-multiedit-prev))

      (:after evil-snipe
        :map evil-snipe-parent-transient-map
        ;; switch to evil-easymotion/avy after a snipe
        "C-;" (λ! (require 'evil-easymotion)
                  (call-interactively
                   (evilem-create #'evil-snipe-repeat
                                  :bind ((evil-snipe-scope 'whole-buffer)
                                         (evil-snipe-enable-highlight)
                                         (evil-snipe-enable-incremental-highlight))))))

      ;; evil-surround
      :v  "S"  #'evil-surround-region
      :o  "s"  #'evil-surround-edit
      :o  "S"  #'evil-Surround-edit

      (:after flycheck
        :map flycheck-error-list-mode-map
        :n "C-n" #'flycheck-error-list-next-error
        :n "C-p" #'flycheck-error-list-previous-error
        :n "j"   #'flycheck-error-list-next-error
        :n "k"   #'flycheck-error-list-previous-error
        :n "RET" #'flycheck-error-list-goto-error)
)

(general-define-key
:states '(normal)
:keymaps 'org-mode-map
:prefix ","
"ji" '(counsel-org-goto :which-key "counsel-org-goto")
"jI" '(counsel-org-goto-all :which-key "counsel-org-goto-all")
"c" '(org-ctrl-c-ctrl-c :which-key "org-ctrl-c-ctrl-c")
"o" '(org-attach-open-in-emacs :which-key "open attachment")
"a" '(org-attach-attach :which-key "attach file")
"l" '(org-store-link :which-key "store link")
"p" '(org-insert-link :which-key "insert link")
"e" '(org-export-dispatch :which-key "export")
"ti" '(org-clock-in :which-key "clock in")
"to" '(org-clock-out :which-key "clock out")
)

(general-define-key
:states '(normal)
:keymaps 'org-mode-map
:prefix "C-c"
"l" '(org-store-link :which-key "org-store-link")
)

;; (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define-global ";;" 'counsel-M-x)
;; (key-chord-define-global ";a" 'move-beginning-of-line)
(key-chord-define-global ";a" 'evil-goto-first-line)
(key-chord-define-global ";b" '+ivy/switch-workspace-buffer)
(key-chord-define-global ";c" 'counsel-find-file)
(key-chord-define-global ";d" '+workspace/close-window-or-workspace)
(key-chord-define-global ";e" 'evil-goto-line)
(key-chord-define-global ";f" 'counsel-find-file)
;; (key-chord-define-global ";g" 'evil-goto-first-line)
(key-chord-define-global ";g" 'magit-status)
(key-chord-define-global ";j" 'dired-jump)
(key-chord-define-global ";k" 'evil-avy-goto-char-2)
(key-chord-define-global ";o" '+lookup/online)
(key-chord-define-global ";p" 'switch-to-previous-buffer)
;; (key-chord-define-global ";s" 'save-buffer)
(key-chord-define-global ";w" 'save-buffer)
(key-chord-define-global ";z" 'move-end-of-line)

;; keyboard shortcuts
;; (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
;; (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
;; (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete)
;; ;; wait until map is available
;; (with-eval-after-load "pdf-annot"
;; (define-key pdf-annot-edit-contents-minor-mode-map (kbd "<return>") 'pdf-annot-edit-contents-commit)
;; (define-key pdf-annot-edit-contents-minor-mode-map (kbd "<S-return>") 'newline)
;; ;; save after adding comment
;; (advice-add 'pdf-annot-edit-contents-commit :after 'bjm/save-buffer-no-args)))
