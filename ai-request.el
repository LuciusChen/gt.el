;;; ai-request.el --- Simplified AI request module -*- lexical-binding: t -*-

;; Copyright (C) 2024 lorniu <lorniu@gmail.com>
;; Extracted and simplified for reuse

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A simplified module for making AI API requests (ChatGPT compatible).
;; Extracted from gt.el for easy reuse in other projects.
;;
;; Usage:
;;   (require 'ai-request)
;;
;;   ;; Basic usage - synchronous request
;;   (ai-request-chat "Explain the word 'serendipity'"
;;     :key "your-api-key"
;;     :done (lambda (content) (message "Result: %s" content)))
;;
;;   ;; With custom system prompt for dictionary
;;   (ai-request-chat "apple"
;;     :key "your-api-key"
;;     :system "You are a dictionary. For each word, provide: 1) pronunciation 2) part of speech 3) definition 4) example sentences"
;;     :done (lambda (content) (message "%s" content)))
;;
;;   ;; Synchronous call
;;   (let ((result (ai-request-chat-sync "hello" :key "your-api-key")))
;;     (message "Result: %s" result))
;;
;;   ;; Streaming output
;;   (ai-request-chat "Tell me a story"
;;     :key "your-api-key"
;;     :stream t
;;     :on-chunk (lambda (chunk) (insert chunk)))

;;; Code:

(require 'json)
(require 'url)

;;; Configuration

(defgroup ai-request nil
  "Configuration for AI request module."
  :group 'applications)

(defcustom ai-request-host "https://api.openai.com"
  "API host for AI service.
Examples:
  - OpenAI: https://api.openai.com
  - Azure: https://YOUR-RESOURCE.openai.azure.com
  - Local: http://localhost:11434 (for Ollama)"
  :type 'string
  :group 'ai-request)

(defcustom ai-request-path "/v1/chat/completions"
  "API endpoint path."
  :type 'string
  :group 'ai-request)

(defcustom ai-request-model "gpt-4o-mini"
  "Model to use for requests."
  :type 'string
  :group 'ai-request)

(defcustom ai-request-key nil
  "API key for authentication.
Can also be set via environment variable OPENAI_API_KEY."
  :type 'string
  :group 'ai-request)

(defcustom ai-request-temperature 0.7
  "Temperature parameter for AI responses (0-2).
Lower = more focused, Higher = more creative."
  :type 'number
  :group 'ai-request)

(defcustom ai-request-timeout 60
  "Request timeout in seconds."
  :type 'integer
  :group 'ai-request)

(defcustom ai-request-system-prompt "You are a helpful assistant."
  "Default system prompt."
  :type 'string
  :group 'ai-request)

(defcustom ai-request-proxy nil
  "HTTP proxy for requests. Format: http://host:port"
  :type '(choice (const nil) string)
  :group 'ai-request)

;;; Utility functions

(defun ai-request--get-key (key)
  "Get API key from KEY argument, config, or environment."
  (or key
      ai-request-key
      (getenv "OPENAI_API_KEY")
      (error "API key not provided. Set `ai-request-key' or OPENAI_API_KEY env var")))

(defun ai-request--build-url (&optional host path)
  "Build full API URL from HOST and PATH."
  (concat (or host ai-request-host)
          (or path ai-request-path)))

(defun ai-request--build-headers (key &optional extra-headers)
  "Build HTTP headers with KEY and EXTRA-HEADERS."
  (let ((headers `(("Content-Type" . "application/json")
                   ("Authorization" . ,(format "Bearer %s" key)))))
    (append headers extra-headers)))

(defun ai-request--build-messages (prompt &optional system)
  "Build messages array from PROMPT and SYSTEM prompt.
PROMPT can be a string or a list of message alists."
  (let ((system-msg `((role . "system")
                      (content . ,(or system ai-request-system-prompt)))))
    (if (stringp prompt)
        (vector system-msg
                `((role . "user") (content . ,prompt)))
      ;; prompt is already a list of messages
      (vconcat (vector system-msg) prompt))))

(defun ai-request--build-body (messages &optional model temperature stream extra-options)
  "Build request body with MESSAGES, MODEL, TEMPERATURE, STREAM and EXTRA-OPTIONS."
  (let ((body `((model . ,(or model ai-request-model))
                (messages . ,messages)
                (temperature . ,(or temperature ai-request-temperature)))))
    (when stream
      (push '(stream . t) body))
    (when extra-options
      (setq body (append body extra-options)))
    body))

;;; Core request functions (using url.el - no external dependencies)

(defun ai-request--parse-response (response-buffer)
  "Parse JSON response from RESPONSE-BUFFER."
  (with-current-buffer response-buffer
    (goto-char (point-min))
    ;; Skip HTTP headers
    (when (re-search-forward "\n\n" nil t)
      (let ((json-object-type 'alist)
            (json-array-type 'vector))
        (condition-case err
            (json-read)
          (error
           (error "Failed to parse response: %s\nBody: %s"
                  (error-message-string err)
                  (buffer-substring (point) (point-max)))))))))

(defun ai-request--extract-content (response)
  "Extract content from parsed RESPONSE."
  (let* ((choices (alist-get 'choices response))
         (choice (and choices (aref choices 0)))
         (message (alist-get 'message choice))
         (content (alist-get 'content message)))
    (or content
        (let ((err (alist-get 'error response)))
          (error "API error: %s" (or (alist-get 'message err) response))))))

;;;###autoload
(cl-defun ai-request-chat (prompt &key
                                   host path model key
                                   system temperature
                                   extra-options headers
                                   stream on-chunk
                                   done fail
                                   timeout)
  "Send a chat request to AI API.

PROMPT is the user message (string) or a list of message alists.

Keyword arguments:
  :host        - API host (default: `ai-request-host')
  :path        - API path (default: `ai-request-path')
  :model       - Model name (default: `ai-request-model')
  :key         - API key (default: `ai-request-key' or env var)
  :system      - System prompt (default: `ai-request-system-prompt')
  :temperature - Temperature 0-2 (default: `ai-request-temperature')
  :extra-options - Additional API options as alist
  :headers     - Extra HTTP headers as alist
  :stream      - Enable streaming (default: nil)
  :on-chunk    - Callback for each chunk when streaming: (lambda (text) ...)
  :done        - Success callback: (lambda (full-content) ...)
  :fail        - Error callback: (lambda (error-message) ...)
  :timeout     - Request timeout in seconds"
  (let* ((api-key (ai-request--get-key key))
         (url (ai-request--build-url host path))
         (messages (ai-request--build-messages prompt system))
         (body (ai-request--build-body messages model temperature stream extra-options))
         (url-request-method "POST")
         (url-request-extra-headers (ai-request--build-headers api-key headers))
         (url-request-data (encode-coding-string (json-encode body) 'utf-8))
         (url-proxy-services (when ai-request-proxy
                              `(("http" . ,ai-request-proxy)
                                ("https" . ,ai-request-proxy)))))
    (if stream
        ;; Streaming request
        (ai-request--stream-request url api-key body on-chunk done fail timeout)
      ;; Non-streaming request
      (url-retrieve
       url
       (lambda (status done-cb fail-cb)
         (if-let ((err (plist-get status :error)))
             (when fail-cb
               (funcall fail-cb (format "Request failed: %s" err)))
           (condition-case err
               (let* ((response (ai-request--parse-response (current-buffer)))
                      (content (ai-request--extract-content response)))
                 (when done-cb
                   (funcall done-cb content)))
             (error
              (if fail-cb
                  (funcall fail-cb (error-message-string err))
                (signal (car err) (cdr err))))))
         (kill-buffer (current-buffer)))
       (list done fail)
       t ; silent
       t ; inhibit cookies
       ))))

;;;###autoload
(cl-defun ai-request-chat-sync (prompt &key
                                        host path model key
                                        system temperature
                                        extra-options headers
                                        timeout)
  "Send a synchronous chat request to AI API.
Returns the response content as a string.

See `ai-request-chat' for argument descriptions."
  (let* ((api-key (ai-request--get-key key))
         (url (ai-request--build-url host path))
         (messages (ai-request--build-messages prompt system))
         (body (ai-request--build-body messages model temperature nil extra-options))
         (url-request-method "POST")
         (url-request-extra-headers (ai-request--build-headers api-key headers))
         (url-request-data (encode-coding-string (json-encode body) 'utf-8))
         (url-proxy-services (when ai-request-proxy
                              `(("http" . ,ai-request-proxy)
                                ("https" . ,ai-request-proxy)))))
    (with-current-buffer (url-retrieve-synchronously url t nil (or timeout ai-request-timeout))
      (unwind-protect
          (let* ((response (ai-request--parse-response (current-buffer)))
                 (content (ai-request--extract-content response)))
            content)
        (kill-buffer (current-buffer))))))

;;; Streaming support

(defun ai-request--stream-request (url api-key body on-chunk done fail timeout)
  "Send streaming request to URL with API-KEY and BODY.
ON-CHUNK is called for each text chunk.
DONE is called with full content when complete.
FAIL is called on error.
TIMEOUT specifies request timeout."
  (let* ((proc-buffer (generate-new-buffer " *ai-stream*"))
         (accumulated "")
         (tracking-pos 1))
    (condition-case err
        (let ((proc (make-network-process
                     :name "ai-stream"
                     :buffer proc-buffer
                     :host (url-host (url-generic-parse-url url))
                     :service (or (url-port (url-generic-parse-url url)) 443)
                     :coding 'utf-8
                     :nowait t
                     :tls-parameters (cons 'gnutls-x509pki
                                          (gnutls-boot-parameters
                                           :hostname (url-host (url-generic-parse-url url)))))))
          ;; Set up sentinel for connection
          (set-process-sentinel
           proc
           (lambda (p event)
             (when (string-match-p "^open" event)
               ;; Send HTTP request
               (let* ((host (url-host (url-generic-parse-url url)))
                      (path (url-filename (url-generic-parse-url url)))
                      (data (encode-coding-string (json-encode body) 'utf-8))
                      (request (format "POST %s HTTP/1.1\r\nHost: %s\r\nContent-Type: application/json\r\nAuthorization: Bearer %s\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
                                      (if (string-empty-p path) "/" path)
                                      host
                                      api-key
                                      (length data)
                                      data)))
                 (process-send-string p request)))
             (when (string-match-p "\\(deleted\\|connection broken\\|finished\\)" event)
               (when done
                 (funcall done accumulated))
               (when (buffer-live-p proc-buffer)
                 (kill-buffer proc-buffer)))))

          ;; Set up filter for receiving data
          (set-process-filter
           proc
           (lambda (_p output)
             (with-current-buffer proc-buffer
               (goto-char (point-max))
               (insert output)
               ;; Parse SSE stream
               (goto-char tracking-pos)
               (while (re-search-forward "^data: \\(.+\\)$" nil t)
                 (let ((data-line (match-string 1)))
                   (unless (string= data-line "[DONE]")
                     (condition-case nil
                         (let* ((json-object-type 'alist)
                                (json-array-type 'vector)
                                (json (json-read-from-string data-line))
                                (choices (alist-get 'choices json))
                                (delta (and choices (alist-get 'delta (aref choices 0))))
                                (content (alist-get 'content delta)))
                           (when (stringp content)
                             (setq accumulated (concat accumulated content))
                             (when on-chunk
                               (funcall on-chunk content))))
                       (error nil))))
                 (setq tracking-pos (point)))))))
      (error
       (when fail
         (funcall fail (error-message-string err)))
       (when (buffer-live-p proc-buffer)
         (kill-buffer proc-buffer))))))

;;; High-level convenience functions

;;;###autoload
(cl-defun ai-request-dictionary (word &key key host model done fail)
  "Look up WORD definition using AI.

Returns a structured dictionary entry with:
- Pronunciation (IPA)
- Part of speech
- Definitions
- Example sentences
- Synonyms/Antonyms

KEY is the API key (optional if configured).
HOST and MODEL override defaults.
DONE callback receives the result.
FAIL callback receives error message."
  (ai-request-chat word
    :key key :host host :model model
    :system "You are a comprehensive dictionary. For each word provided, give:
1. **Pronunciation**: IPA notation
2. **Part of Speech**: (noun, verb, adj, etc.)
3. **Definitions**: Clear, numbered definitions
4. **Examples**: 2-3 example sentences for each main definition
5. **Synonyms**: Related words
6. **Etymology**: Brief word origin (if interesting)

Format the output clearly with markdown."
    :done done
    :fail fail))

;;;###autoload
(cl-defun ai-request-dictionary-sync (word &key key host model)
  "Synchronously look up WORD definition using AI.
Returns the dictionary entry as a string.

See `ai-request-dictionary' for argument descriptions."
  (ai-request-chat-sync word
    :key key :host host :model model
    :system "You are a comprehensive dictionary. For each word provided, give:
1. **Pronunciation**: IPA notation
2. **Part of Speech**: (noun, verb, adj, etc.)
3. **Definitions**: Clear, numbered definitions
4. **Examples**: 2-3 example sentences for each main definition
5. **Synonyms**: Related words
6. **Etymology**: Brief word origin (if interesting)

Format the output clearly with markdown."))

;;;###autoload
(cl-defun ai-request-translate (text target-lang &key key host model source-lang done fail)
  "Translate TEXT to TARGET-LANG.

TARGET-LANG should be a language name like \"Chinese\", \"Japanese\", etc.
SOURCE-LANG is optional source language hint.
KEY, HOST, MODEL override defaults.
DONE and FAIL are callbacks."
  (let ((prompt (if source-lang
                    (format "Translate from %s to %s:\n\n%s" source-lang target-lang text)
                  (format "Translate to %s:\n\n%s" target-lang text))))
    (ai-request-chat prompt
      :key key :host host :model model
      :system "You are a professional translator. Translate the given text accurately while preserving the original meaning, tone, and style. Only output the translation without any explanation."
      :done done
      :fail fail)))

;;;###autoload
(cl-defun ai-request-translate-sync (text target-lang &key key host model source-lang)
  "Synchronously translate TEXT to TARGET-LANG.
Returns the translation as a string.

See `ai-request-translate' for argument descriptions."
  (let ((prompt (if source-lang
                    (format "Translate from %s to %s:\n\n%s" source-lang target-lang text)
                  (format "Translate to %s:\n\n%s" target-lang text))))
    (ai-request-chat-sync prompt
      :key key :host host :model model
      :system "You are a professional translator. Translate the given text accurately while preserving the original meaning, tone, and style. Only output the translation without any explanation.")))

;;;###autoload
(cl-defun ai-request-explain (text &key key host model context done fail)
  "Explain TEXT (code, concept, or content).

CONTEXT provides additional context for explanation.
KEY, HOST, MODEL override defaults.
DONE and FAIL are callbacks."
  (let ((prompt (if context
                    (format "Context: %s\n\nPlease explain:\n%s" context text)
                  (format "Please explain:\n%s" text))))
    (ai-request-chat prompt
      :key key :host host :model model
      :system "You are a knowledgeable teacher. Explain concepts clearly and concisely. Use examples when helpful. If it's code, explain what it does step by step."
      :done done
      :fail fail)))

(provide 'ai-request)

;;; ai-request.el ends here
