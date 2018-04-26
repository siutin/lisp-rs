
# scheme-rs
A Scheme Interpreter written in Rust based on Peter Norvig's lispy (http://norvig.com/lispy.html)

<p align="center">
  <a href="https://travis-ci.org/siutin/scheme-rs" alt="Build Status">
    <img src="https://travis-ci.org/siutin/scheme-rs.svg?branch=master"/>
  </a>
  <a href="https://app.fossa.io/projects/git%2Bgithub.com%2Fsiutin%2Fscheme-rs?ref=badge_shield" alt="FOSSA Status">
    <img src="https://app.fossa.io/api/projects/git%2Bgithub.com%2Fsiutin%2Fscheme-rs.svg?type=shield"/>
  </a>
</p>

## Quick Start
Try REPL: 

```
> cargo run --release --bin cli

Welcome to scheme-rs
scheme=> (+ 1 2 (* 3 4 5) 6 7 (/ 8 9 10))
Number(76.08888888888889)
scheme=> 

```

Run scheme code from a file using the interpreter:

```
# using cargo
> cargo run --release --bin scheme -- ./examples/demo_01.scm

# or executing from the build
> scheme ./examples/demo_01.scm
```

## Building
```

cargo build --release --bin scheme       # interpreter

cargo build --release --bin cli          # interactive shell

```


## License
[![FOSSA Status](https://app.fossa.io/api/projects/git%2Bgithub.com%2Fsiutin%2Fscheme-rs.svg?type=large)](https://app.fossa.io/projects/git%2Bgithub.com%2Fsiutin%2Fscheme-rs?ref=badge_large)
