# CL-Coveralls

CL-Coveralls is a helper library to post test coverage to [Coveralls](https://coveralls.io).

## Warning

This software is still ALPHA quality. The APIs will be likely to change.

## Limitations

### Supported implementations

* SBCL
* Clozure CL 1.4-1.9 (Not support 1.10 because it's broken)

### Supported CI services

* [Travis CI](https://travis-ci.org)
* [CircleCI](https://circleci.com)

### Number of entered

Coveralls allows us to track the number of entered times for each lines, however all Common Lisp coverage tools don't provide the information. CL-Coveralls posts it as `1` for all lines entered.

## Usage

CL-Coveralls see if `$COVERALLS` is bound for deciding whether recording test coverage or not. Don't forget to set the environment variable before running tests.

```
$ COVERALLS=true sbcl --load test-script.lisp --eval '(sb-ext:exit)'
```

In your test script, wrap your test code with `coveralls:with-coveralls`.

```common-lisp
(coveralls:with-coveralls ()
  ;; Run tests
  (prove:run :your-app))
```

## Examples

* [Lack's .travis.yml](https://github.com/fukamachi/lack/blob/master/.travis.yml)

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the BSD 2-Clause License.
