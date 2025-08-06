%{
  # Credo configuration aligned with Riva Ash style guide
  # Enforces functional programming principles, single level of abstraction,
  # and high code quality standards as defined in docs/styleguide.md
  #
  configs: [
    %{
      name: "default",
      strict: true,  # Always use strict mode for consistent high-quality code
      files: %{
        # Include these directories and files:
        included: [
          "lib/",
          "src/",
          "web/",
          "apps/*/lib/",
          "apps/*/src/",
          "apps/*/web/",
          "test/",
          "config/"
        ],
        # Exclude these directories and files:
        excluded: [
          "deps/",
          "_build/",
          "assets/node_modules/",
          "priv/static/",
          "uploads/",
          "cover/",
          "doc/"
        ]
      },
      checks: [
        # ** Consistency Checks - Enforce consistent code style
        {Credo.Check.Consistency.ExceptionNames},
        {Credo.Check.Consistency.LineEndings},
        {Credo.Check.Consistency.UnusedVariableNames},
        {Credo.Check.Consistency.SpaceAroundOperators},
        {Credo.Check.Consistency.SpaceInParentheses},
        {Credo.Check.Consistency.TabsOrSpaces},

        # ** Design Checks - Enforce architectural principles
        {Credo.Check.Design.AliasUsage,
         # Prevent excessive alias usage that obscures code clarity
         if_nested_deeper_than: 1, if_called_more_often_than: 1},
        {Credo.Check.Design.DuplicatedCode, false},  # Disabled due to compatibility issues
        {Credo.Check.Design.TagFIXME, exit_status: 1},  # Warn on FIXME
        {Credo.Check.Design.TagTODO, exit_status: 0},   # Allow TODO but warn

        # ** Readability Checks - Enforce clear, readable code
        {Credo.Check.Readability.AliasAs},
        {Credo.Check.Readability.AliasOrder},
        {Credo.Check.Readability.BlockPipe},
        {Credo.Check.Readability.FunctionNames},
        {Credo.Check.Readability.LargeNumbers},
        {Credo.Check.Readability.MaxLineLength, max_length: 120},  # Match formatter config
        {Credo.Check.Readability.ModuleAttributeNames},
        {Credo.Check.Readability.ModuleDoc,
         # Require documentation for public modules (except test modules)
         ignore_names: ~r/Test$|Mock$|Fixture$|TestHelper$/},
        {Credo.Check.Readability.ModuleNames},
        {Credo.Check.Readability.ParenthesesOnZeroArityDefs},
        {Credo.Check.Readability.ParenthesesInCondition},
        {Credo.Check.Readability.PredicateFunctionNames},
        {Credo.Check.Readability.PreferImplicitTry},
        {Credo.Check.Readability.StringSigils},
        {Credo.Check.Readability.TrailingBlankLine},
        {Credo.Check.Readability.TrailingWhiteSpace},
        {Credo.Check.Readability.VariableNames},
        {Credo.Check.Readability.WithSingleClause},

        # ** Refactor Checks - Enforce style guide principles
        # Aligned with "Single Level of Abstraction" and functional programming
        {Credo.Check.Refactor.CyclomaticComplexity, max_complexity: 6},  # Force simple functions
        {Credo.Check.Refactor.FunctionArity, max_arity: 3},              # Encourage small functions
        {Credo.Check.Refactor.MatchInCondition},                         # Prefer pattern matching
        {Credo.Check.Refactor.Nesting, max_nesting: 2},                  # Force pattern matching over nesting
        {Credo.Check.Refactor.PipeChainStart},                           # Enforce proper pipeline usage
        {Credo.Check.Refactor.RejectReject},                             # Avoid double negatives
        {Credo.Check.Refactor.UnlessWithElse},                           # Prefer if over unless with else

        # ** Warnings - Catch potential runtime issues
        {Credo.Check.Warning.IExPry},                    # No debug code in production
        {Credo.Check.Warning.LazyLogging},               # Proper logging practices
        {Credo.Check.Warning.OperationWithConstantResult},
        {Credo.Check.Warning.RaiseInsideRescue},         # Avoid masking errors
        {Credo.Check.Warning.UnsafeExec},                # Security: no unsafe exec
        {Credo.Check.Warning.UnsafeToAtom},              # Security: prevent atom exhaustion
        {Credo.Check.Warning.UnusedEnumOperation},       # Catch unused operations
        {Credo.Check.Warning.UnusedFileOperation},
        {Credo.Check.Warning.UnusedKeywordOperation},
        {Credo.Check.Warning.UnusedListOperation},
        {Credo.Check.Warning.UnusedPathOperation},
        {Credo.Check.Warning.UnusedRegexOperation},
        {Credo.Check.Warning.UnusedStringOperation},
        {Credo.Check.Warning.UnusedTupleOperation}
      ]
    }
  ]
}
