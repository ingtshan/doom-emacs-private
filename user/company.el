;;; user/company.el -*- lexical-binding: t; -*-

(defun company-complete-if-selected ()
  (interactive)
  (if (eq company-selection nil) (newline-and-indent) (company-complete)))

(after! ruby-mode
  (defconst ruby-common-words
    '("deep_symbolize_keys" "deep_stringify_keys" "greater_than" "to_json" "valid?" "invalid?"
      "greater_than_or_equal_to" "equal_to" "less_than" "less_than_or_equal_to"
      "other_than" "any?" "assoc" "clear" "Time.zone.now" "Date.today" "present?" "blank?" "nil?"
      "compact" "compact!" "compare_by_identity" "compare_by_identity?"
      "deconstruct_keys" "default" "default=" "default_proc"
      "default_proc=" "delete" "delete_if" "dig"
      "each" "each_key" "each_pair" "each_value"
      "empty?" "eql?" "except" "fetch"
      "fetch_values" "filter" "filter!" "flatten"
      "has_key?" "has_value?" "hash" "include?"
      "initialize_copy" "inspect" "invert" "keep_if"
      "key" "key?" "keys" "length"
      "member?" "merge" "merge!" "rassoc"
      "rehash" "reject" "reject!" "replace"
      "select" "select!" "shift" "size"
      "slice" "store" "to_a" "to_h"
      "to_hash" "to_proc" "to_s" "transform_keys"
      "transform_keys!" "transform_values" "transform_values!" "update"
      "value?" "values" "values_at" "all?"
      "append" "at" "bsearch" "bsearch_index"
      "collect" "collect!" "combination" "concat"
      "count" "cycle" "deconstruct" "delete_at"
      "difference" "drop" "drop_while" "each_index"
      "fill" "find_index" "first" "flatten!"
      "index" "insert" "intersection" "join"
      "last" "map" "map!" "max"
      "min" "minmax" "none?" "old_to_s"
      "one?" "pack" "permutation" "pop"
      "prepend" "product" "push" "repeated_combination"
      "repeated_permutation" "reverse" "reverse!" "reverse_each"
      "rindex" "rotate" "rotate!" "sample"
      "shuffle" "shuffle!" "slice!" "sort"
      "sort!" "sort_by!" "sum" "take"
      "take_while" "to_ary" "transpose" "union"
      "uniq" "uniq!" "unshift" "zip"
      "ascii_only?" "bytes" "bytesize" "byteslice"
      "capitalize" "capitalize!" "casecmp" "casecmp?"
      "center" "chars" "chomp" "chomp!"
      "chop" "chop!" "chr" "codepoints"
      "crypt" "delete!" "delete_prefix" "delete_prefix!"
      "delete_suffix" "delete_suffix!" "downcase" "downcase!"
      "dump" "each_byte" "each_char" "each_codepoint"
      "each_grapheme_cluster" "each_line" "encode" "encode!"
      "encoding" "end_with?" "force_encoding" "freeze"
      "getbyte" "grapheme_clusters" "gsub" "gsub!"
      "hex" "intern" "lines" "ljust"
      "lstrip" "lstrip!" "match" "match?"
      "next" "next!" "oct" "ord"
      "partition" "rjust" "rpartition" "rstrip"
      "rstrip!" "scan" "scrub" "scrub!"
      "setbyte" "split" "squeeze" "squeeze!"
      "start_with?" "strip" "strip!" "sub"
      "sub!" "succ" "succ!" "swapcase"
      "swapcase!" "to_c" "to_f" "to_i"
      "to_r" "to_str" "to_sym" "tr"
      "tr!" "tr_s" "tr_s!" "undump"
      "unicode_normalize" "unicode_normalize!" "unicode_normalized?" "unpack"
      "unpack1" "upcase" "upcase!" "upto"
      "valid_encoding?" "ajd" "amjd" "asctime"
      "ctime" "cwday" "cweek" "cwyear"
      "day" "day_fraction" "downto" "england"
      "friday?" "gregorian" "gregorian?" "httpdate"
      "infinite?" "inspect_raw" "iso8601" "italy"
      "jd" "jisx0301" "julian" "julian?"
      "ld" "leap?" "marshal_dump_old" "mday"
      "mjd" "mon" "monday?" "month"
      "new_start" "next_day" "next_month" "next_year"
      "nth_kday?" "prev_day" "prev_month" "prev_year"
      "rfc2822" "rfc3339" "rfc822" "saturday?"
      "start" "step" "strftime" "strftime('%Y-%m-%d')" "strftime('%d/$m/%Y')" "sunday?"
      "thursday?" "to_date" "to_datetime" "to_time"
      "tuesday?" "wday" "wednesday?" "xmlschema"
      "acceptance" "validates_associated" "confirmation"
      "exclusion" "format" "inclusion" "perform_later" "perform_now" "set" "perform"
      "numericality: " "presence: true" "presence: " "absence" "uniqueness" "allow_nil" "allow_blank" "message"
      "uniqueness: true" "uniqueness: " "allow_nil: true" "allow_nil: " "allow_blank: true" "allow_blank: " "message: " "on: "
      "yday" "year" "optional: false" "optional: true" "errors.full_messages.to_sentence" "before_action" "before_action :" "skip_before_action :" "protect_from_forgery with: :" "rescue_from :" "with: "
      "acts_like_date?"
      "advance"
      "ago"
      "at_beginning_of_day"
      "at_end_of_day"
      "at_midday"
      "at_middle_of_day"
      "at_midnight"
      "at_noon"
      "beginning_of_day"
      "beginning_of_week"
      "compare_with_coercion"
      "compare_without_coercion"
      "current"
      "default_inspect"
      "end_of_day"
      "find_beginning_of_week!"
      "midday"
      "middle_of_day"
      "midnight"
      "noon"
      "readable_inspect"
      "since"
      "to_time"
      "tomorrow"
      "yesterday"
      )
    )
  (defconst rspec-common-words
    '("actual"
      "actual_exists?"
      "add_should_and_should_not_to"
      "and_return"
      "allow"
      "aggregate_failures"
      "aggregation_block_label"
      "aggregation_metadata"
      "lias_matcher"
      "all"
      "all_exceptions"
      "and"
      "argument"
      "at_least"
      "at_most"
      "backtrace_formatter"
      "be"
      "be_a"
      "be_a_kind_of"
      "be_an_instance_of"
      "be_between"
      "be_falsey"
      "be_nil"
      "be_truthy"
      "be_within"
      "block_arg"
      "by"
      "by_at_least"
      "by_at_most"
      "captures"
      "chain"
      "change"
      "lear_generated_description"
      "color?"
      "onfiguration"
      "contain_exactly"
      "cover"
      "efault_should_host"
      "define"
      "efine_negated_matcher"
      "description"
      "description_of"
      "diffable"
      "diffable?"
      "isable_expect"
      "isable_should"
      "does_not_match?"
      "nable_expect"
      "nable_should"
      "end_with"
      "eq"
      "eql"
      "equal"
      "exactly"
      "exception_count_description"
      "exclusive"
      "exist"
      "expect"
      "xpect_enabled?"
      "expected"
      "expected_as_array"
      "expects_call_stack_jump?"
      "fail"
      "fail_including"
      "ail_with"
      "fail_with"
      "failure_message"
      "failure_message_for_should"
      "failure_message_for_should_not"
      "failure_message_when_negated"
      "failures"
      "or_many_matchers"
      "rom"
      "from"
      "enerated_description"
      "have_attributes"
      "include"
      "include_chain_clauses_in_custom_matcher_descriptions?"
      "inclusive"
      "indeterminate_actual_indexes"
      "indeterminate_expected_indexes"
      "initialize"
      "inspect"
      "ist"
      "match"
      "match_array"
      "match_for_should"
      "match_for_should_not"
      "match_unless_raises"
      "match_when_negated"
      "matcher_matches?"
      "matches?"
      "message"
      "message_with_diff"
      "method_missing"
      "name"
      "names"
      "not_to"
      "of"
      "on_potential_false_positives"
      "once"
      "or"
      "other_errors"
      "output"
      "percent_of"
      "raise_error"
      "rescued_exception"
      "respond_to"
      "respond_to?"
      "respond_to_missing?"
      "satisfy"
      "should"
      "hould_enabled?"
      "hould_enumerate?"
      "should_not"
      "plit_words"
      "start_with"
      "summary"
      "supports_block_expectations"
      "supports_block_expectations?"
      "urface_descriptions_in"
      "syntax"
      "syntax="
      "target"
      "thrice"
      "throw_symbol"
      "times"
      "to"
      "to_stderr"
      "to_stderr_from_any_process"
      "to_stdout"
      "to_stdout_from_any_process"
      "twice"
      "unmatched_actual_indexes"
      "unmatched_expected_indexes"
      "nreadable_io?"
      "valid_test?"
      "validity_message"
      "values_match?"
      "warn_about_potential_false_positives="
      "warn_about_potential_false_positives?"
      "arn_about_should!"
      "arn_about_should_unless_configured"
      "with"
      "with_any_keywords"
      "with_captures"
      "with_keywords"
      "with_message"
      "with_unlimited_arguments"
      "yield_control"
      "yield_successive_args"
      "yield_with_args"
      "yield_with_no_args"
      "instance_double"
      "be_present"
      "be_blank"
      "be_valid"
      "be_invalid"))

  (defun company-rspec-backend (command &optional arg &rest ignored)
    (interactive (list 'interactive))

    (cl-case command
      (interactive (company-begin-backend 'company-ruby-backend))
      (prefix (and (and (boundp 'rspec-mode) rspec-mode)
                   (company-grab-symbol)))

      (candidates
       (all-completions arg rspec-common-words))))

  (defun company-ruby-backend (command &optional arg &rest ignored)
    (interactive (list 'interactive))

    (cl-case command
      (interactive (company-begin-backend 'company-ruby-backend))
      (prefix (and (or (eq major-mode 'ruby-mode) (eq major-mode 'inf-ruby-mode))
                   (company-grab-symbol)))

      (candidates
       (all-completions arg ruby-common-words)))))

(add-hook 'ruby-mode-hook
          (lambda ()
            (setq-local +lsp-company-backends '(:separate company-capf company-dabbrev-code company-ruby-backend company-rspec-backend company-yasnippet))
            (setq-local company-transformers '(remove-company-duplicates))))

(after! company
  (setq company-dabbrev-downcase nil)
  (setq company-show-numbers t)
  (setq company-dabbrev-code-everywhere nil)
  (setq company-dabbrev-code-other-buffers t)
  (setq company-idle-delay 0)

  (defun remove-company-duplicates (candidates)
    "Order the snippets / text depending of priority. CANDIDATES: Company candidates."
    (if (< (length candidates) 200)
        (let* ((new-list '()))
          (dolist (candidate candidates)
            (let* ((stripped-candidate (substring-no-properties candidate)))
              (if (and (not (string= (substring stripped-candidate 0 1) ":"))
                       (not (-contains? (mapcar 'substring-no-properties new-list) stripped-candidate))) (push candidate new-list))))
          (reverse new-list)) candidates)))

(after! company
  (setq company-dabbrev-downcase nil)
  (setq company-show-numbers t)
  (setq company-dabbrev-code-everywhere nil)
  (setq company-dabbrev-code-other-buffers t)
  (setq company-idle-delay 0))

(defun better-dabbrev-expand ()
  (interactive)
  (call-interactively 'dabbrev-expand)
  (company-abort))

(defun call-real-ret ()
  (interactive)
  (when company-selection (company-abort))
  (funcall (key-binding (kbd "RET"))))

(map! :i "<C-return>" 'better-dabbrev-expand)
(map! :i "M-RET" 'call-real-ret)
(map! :i "TAB" 'yas-expand)

(defun better-yas-expand ()
  (interactive)
  (if yas--active-snippets (select-and-yas-next) (yas-expand)))

(map! :after company
      :map company-active-map
      "TAB" 'better-yas-expand
      "<tab>" #'better-yas-expand
      "C-e" #'emmet-expand-yas
      "M-RET" #'call-real-ret
      "S-TAB" 'company-complete-selection
      "<C-return>" 'better-dabbrev-expand)

(after! company
  (setq company-dabbrev-code-everywhere t)
  (set-company-backend! 'inf-ruby-mode '(:separate company-dabbrev-code company-capf company-ruby-backend)))

(after! yasnippet
  (defun select-and-yas-next ()
    (interactive)
    (if (eq company-selection nil)
        (yas-next-field)
      (progn (company-abort) (yas-next-field))))

  (defun select-and-yas-previous ()
    (interactive)
    (if (eq company-selection nil)
        (yas-prev-field)
      (progn (company-abort) (yas-prev-field))))

  (defun emmet-expand-line ()
    (interactive)
    (if (eq major-mode 'ruby-mode)
        (otavio/grb)
      (emmet-expand-yas)))

  (map! :map yas-keymap
        "TAB" #'select-and-yas-next
        "S-TAB" #'select-and-yas-previous
        "C-d" #'yas-skip-and-clear-field
        "C-e" #'emmet-expand-yas))
