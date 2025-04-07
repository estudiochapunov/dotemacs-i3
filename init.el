;;; -*- lexical-binding: t -*-

;;; ----------------------------------------------------------------------
;;; 1. Package Management
;;; ----------------------------------------------------------------------

;;; Configura los repositorios de paquetes.
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

;; Inicializa el sistema de paquetes.
(package-initialize)

;; Instala `use-package` si no está instalado.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Carga `use-package` y asegura la instalación automática de los paquetes.
(require 'use-package)
(setq use-package-always-ensure t)

;;; ----------------------------------------------------------------------
;;; 2. Basic UI
;;; ----------------------------------------------------------------------

;; Configura elementos básicos de la interfaz de usuario.

;; Desactiva el mensaje de bienvenida al inicio.
(setq inhibit-startup-message t)

;; Activa las barras de desplazamiento.
(scroll-bar-mode 1)

;; Activa la barra de herramientas.
(tool-bar-mode 1)

;; Activa la barra de menú.
(menu-bar-mode 1)

;; Activa la visualización del número de columna.
(column-number-mode 1)

;; Desactiva la visualización del número de línea globalmente.
(global-display-line-numbers-mode -1)

;;; ----------------------------------------------------------------------
;;; 3. Appearance
;;; ----------------------------------------------------------------------

;; Configura temas y fuentes.

;; Doom themes: Temas visuales.
(use-package doom-themes
  :config
  (load-theme 'doom-one t) ; Carga el tema 'doom-one'.
  (setq doom-themes-enable-bold nil)   ; Desactiva la negrita en el tema.
  (setq doom-themes-enable-italic nil)) ; Desactiva la cursiva en el tema.

;; Configuración de la fuente por defecto.
(set-face-attribute 'default nil
                    :family "SauceCodePro Nerd Font Mono" ; Fuente a usar.
                    :height 110                  ; Altura de la fuente.
                    :weight 'normal               ; Peso de la fuente.
                    :width 'normal)                ; Ancho de la fuente.

;;; ----------------------------------------------------------------------
;;; 4. Navigation
;;; ----------------------------------------------------------------------

;; Hyperbole: Navegación avanzada y gestión de la información.
(use-package hyperbole
  :ensure t
  :config
  (hyperbole-mode 1)
  (setq hywiki-directory (expand-file-name "wiki" hbmap:dir-user))
  (unless (file-exists-p hywiki-directory)
      (make-directory hywiki-directory t))
  (hywiki-mode 1)
  (setq hyperbole-contact-notebook-mode 'hyrolo)
  (setq hsys-org-enable-smart-keys t)
  (setq hyrolo-date-format "%Y-%m-%d %H:%M:%S")
  )

;;; ----------------------------------------------------------------------
;;; 5. Autocompletion
;;; ----------------------------------------------------------------------

;; Company: Marco de trabajo para la autocompletación.
(use-package company
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.2)           ; Retardo antes de que se inicie la autocompletación.
  (company-minimum-prefix-length 1)) ; Longitud mínima del prefijo para la autocompletación.

;; Corfu: Marco de trabajo para la finalización en el búfer.
(use-package corfu
  :custom
  (corfu-auto t)         ; Activa la finalización automática.
  (corfu-auto-delay 0.2)   ; Establece el retardo para la finalización automática.
  :init
  (global-corfu-mode))

;;; ----------------------------------------------------------------------
;;; 6. Parenthesis and Syntax
;;; ----------------------------------------------------------------------

;; Smartparens: Manipulación inteligente de paréntesis y otros pares.
(use-package smartparens
  :ensure t
  :init
  (require 'smartparens-config)
  :config
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  (sp-use-paredit-bindings)
  (setq sp-show-pair-from-inside t)
  (sp-local-pair '(emacs-lisp-mode lisp-mode scheme-mode) "'" nil :actions nil))

;; Rainbow delimiters: Colorea los delimitadores según su anidamiento.
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Lispy: Edición de código Lisp-like.
(use-package lispy
  :ensure t
  :hook (emacs-lisp-mode . lispy-mode))

;;; ----------------------------------------------------------------------
;;; 7. Language-specific
;;; ----------------------------------------------------------------------

;; SLY: Soporte para Common Lisp.
(use-package sly
  :custom
  (inferior-lisp-program "sbcl") ; Programa Lisp a usar.
  :config
  (define-key sly-mode-map (kbd "C-c C-c") 'sly-eval-defun) ; Evaluar la definición de la función.
  (define-key sly-mode-map (kbd "C-c C-k") 'sly-compile-and-load-file)) ; Compilar y cargar el archivo.

;; Geiser: Soporte para Scheme/Racket.
(use-package geiser
  :ensure t
  :config
  (setq geiser-active-implementations '(racket))) ; Implementaciones de Scheme a usar.

;; Modo Geiser para Racket.
(use-package geiser-racket
  :ensure t
  :after geiser)

;;; ----------------------------------------------------------------------
;;; 8. Project and Version Control
;;; ----------------------------------------------------------------------

;; Projectile: Gestión de proyectos.
(use-package projectile
  :init
  (projectile-mode +1)
  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map)))

;; Magit: Interfaz para Git.
(use-package magit
  :bind
  ("C-x g" . magit-status))

;;; ----------------------------------------------------------------------
;;; 9. Terminal
;;; ----------------------------------------------------------------------

;; Vterm: Emulador de terminal.
(use-package vterm
  :load-path "~/.emacs.d/site-lisp/emacs-libvterm" ; Ruta para la biblioteca de Vterm.
  :config
  (setq vterm-shell "/bin/bash")        ; Shell a usar.
  (setq vterm-max-scrollback 10000)   ; Número máximo de líneas para el historial.
  :bind (("C-c v" . vterm)              ; Abrir Vterm.
         ("C-c C-v" . vterm-other-window))) ; Abrir Vterm en otra ventana.

;;; ----------------------------------------------------------------------
;;; 10. System Integration
;;; ----------------------------------------------------------------------

;; Xclip: Integración con el portapapeles de X.
(use-package xclip
  :config
  (xclip-mode 1)
  (setq xclip-program "xclip")
  (setq xclip-select-enable-clipboard t))

;;; ----------------------------------------------------------------------
;;; 11. Search
;;; ----------------------------------------------------------------------

;; Consult: Utilidades de búsqueda mejoradas.
(use-package consult
  :bind
  (("C-S" . consult-line)     ; Buscar en la línea actual.
   ("C-R" . consult-ripgrep)  ; Buscar en archivos usando ripgrep.
   ("M-y" . consult-yank-pop) ; Historial del portapapeles.
   ("C-x b" . consult-buffer)  ; Cambiar de búfer.
   :map minibuffer-local-map  ; Mapa de teclas local del minibúfer.
   ("C-H m" . consult-history))) ; Historial del minibúfer.

;; Embark: Acciones contextuales sobre los objetivos.
(use-package embark
  :bind
  (("C-." . embark-act)      ; Acción principal.
   ("C-;" . embark-dwim)     ; Acción "Do What I Mean".
   ("C-h B" . embark-bindings))) ; Mostrar las combinaciones de teclas.

;; Integración de Embark y Consult.
(use-package embark-consult
  :after (embark consult))

;;; ----------------------------------------------------------------------
;;; 12. Other
;;; ----------------------------------------------------------------------

;; Otras configuraciones y paquetes.

(use-package json-process-client
  :ensure t)

(use-package indium
  :ensure t)

;; EAF: Emacs Application Framework.
(use-package eaf
  :load-path "~/.emacs.d/eaf")

;;; ----------------------------------------------------------------------
;;; 13. Custom Functions
;;; ----------------------------------------------------------------------

;; Funciones personalizadas.

;; Configuración para los modos Lisp.
(defun my-lisp-mode-setup ()
  "Configuración para los modos Lisp."
  (interactive)
  (show-paren-mode 1)         ; Resaltar paréntesis coincidentes.
  (smartparens-strict-mode 1) ; Modo estricto de Smartparens.
  (company-mode 1)            ; Activar Company para la autocompletación.
  (display-line-numbers-mode 1) ; Mostrar números de línea.
  (rainbow-delimiters-mode 1))  ; Activar Rainbow Delimiters.

(add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-setup)
(add-hook 'lisp-mode-hook 'my-lisp-mode-setup)
(add-hook 'scheme-mode-hook 'my-lisp-mode-setup)
(add-hook 'sly-mrepl-mode-hook 'my-lisp-mode-setup)
(add-hook 'geiser-repl-mode-hook 'my-lisp-mode-setup)

;; EAT: Emacs ASCII Terminal (configuración alternativa).
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-eat")
(require 'eat nil t)

;; Función para actualizar la configuración de Emacs en GitHub.
(defun my/update-config ()
  "Actualiza la configuración de Emacs en GitHub usando Magit."
  (interactive)
  (let ((emacs-config-dir (or (and (boundp 'user-emacs-directory) user-emacs-directory)
                              (expand-file-name "~/.emacs.d/"))))
    (unless emacs-config-dir
      (error "No se pudo determinar el directorio de configuración de Emacs."))
    (unless (executable-find "git")
      (error "Git no está instalado o no está en el PATH."))
    (save-some-buffers t)

    ;; Verificar si Magit está disponible
    (if (not (fboundp 'magit-status))
        (error "Magit no está instalado. Por favor, instala Magit primero.")

      ;; Abrir Magit en el directorio de configuración
      (cd emacs-config-dir)
      ;; Verificar si es un repositorio Git
      (if (not (file-exists-p (expand-file-name ".git" emacs-config-dir)))
          (error "El directorio de configuración no es un repositorio Git. Inicializa git primero.")

        ;; Mostrar estado de Git
        (magit-status emacs-config-dir)
        (message "Usa Magit para stage (s), commit (c c), y push (P p).")

        ;; Automatización opcional (si se confirma)
        (when (y-or-n-p "¿Quieres stagear todos los cambios (agregar todos los archivos modificados) y hacer commit automáticamente? ")
          (magit-stage-modified t)
          ;; Solicitar mensaje de commit y después crear el commit
          (let ((commit-message (read-string "Mensaje de commit: ")))
            (magit-commit-create (list "-m" commit-message))
            ;; Hacer push
            (when (y-or-n-p "¿Hacer push a GitHub? ")
              (magit-push-current-to-pushremote nil))))))))

;;; ----------------------------------------------------------------------
;;; 14. Final Optimizations
;;; ----------------------------------------------------------------------

;; Configuración final para mejorar el rendimiento.
(setq jit-lock-defer-time 0.05)         ; Retraso para jit-lock.
(setq auto-window-vscroll nil)        ; Desactivar el desplazamiento automático de la ventana.
(setq fast-but-imprecise-scrolling t) ; Activar el desplazamiento rápido pero impreciso.

;;; ----------------------------------------------------------------------
;;; 15. Provide this file as a module
;;; ----------------------------------------------------------------------

;; Proporcionar este archivo como módulo.
(provide 'init)
