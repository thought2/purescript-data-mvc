purs_args := "--stash --censor-lib --censor-codes=WildcardInferredType,ImplicitQualifiedImport,HidingImport"
cfg_test := "--config test.dhall"

build-strict:
    spago build --purs-args "--strict {{purs_args}}"

build:
    spago build --purs-args "{{purs_args}}"

test-strict:
    spago {{cfg_test}} test --purs-args "--strict {{purs_args}}"

test:
    spago {{cfg_test}} test --purs-args "{{purs_args}}"

clean:
    rm -rf .spago output .psa-stash

ide:
    spago {{cfg_test}} test --purs-args "{{purs_args}} --json-errors"

format:
    purs-tidy format-in-place 'src/**/*.purs'
    purs-tidy format-in-place 'test/**/*.purs'

check-format:
    purs-tidy check 'src/**/*.purs'
    purs-tidy check 'test/**/*.purs'

check-git-clean:
    [ -z "$(git status --porcelain)" ]

ci: check-format build-strict test-strict check-git-clean

ci-nix:
    nix develop --command just ci

suggest-list:
    just ide 2>&1 | ps-suggest --list

suggest-apply:
    just ide 2>&1 | ps-suggest --apply
