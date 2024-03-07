# Diffcessible: An accessible diff viewer

Diffcessible is an terminal based diff viewer with an emphasis on being accessible. Our goal is to build a diff viewer on top of Git diff that is accessible to screen readers.

## Overview

The following describes the structure of the project:

`bin/`: The bin directory contains the main entry point for the application and contains the implementation for the command-line interface.
`lib/`: The lib directory contains the main logic for the diff viewer. It is responsible for parsing the diff and displaying it in the terminal.
`test/`: The test directory contains the test suite for the project.
`vendor/`: The vendor directory contains the vendored dependencies for the project.

## Setup Dependencies

To set up the project, please fork the repository and clone it to your local machine. You will need to have `opam` installed to install the dependencies. Once you have `opam` installed, you can run the following command to install the dependencies:

```bash
opam install . --deps-only --with-test
```

## Running the Project

This project runs using the output of a git diff command. Such as:
```bash
git diff > git.diff
```

To run the project, you can use the following command:

```bash
dune exec diffcessible [path/to/git.diff]
```

## Technologies Used

The following is a list of the technologies used in the project:

- [**Patch Library**](https://github.com/hannesm/patch): For parsing and printing diffs.https://github.com/let-def/lwd
- **[Notty](https://github.com/pqwy/notty/) & [Lwd](https://github.com/let-def/lwd) Libraries**: For terminal layout declaration and creating a reactive UI.
- **[Nottui Library](https://github.com/let-def/lwd/tree/master/lib/nottui)**: Integrates Lwd and Notty for UI development.
- **[Cmdliner Library](https://github.com/dbuenzli/cmdliner)**: Facilitates command-line interface creation.
- **[Dune Build System](https://github.com/ocaml/dune)**: For project building.

## Contributing

Contributions are welcome! Please refer to the [CONTRIBUTING.md](CONTRIBUTING.md) file for detailed instructions on how to contribute to this project.

## Learning Resources

For those new to OCaml or looking to enhance their skills, here are some recommended resources:

- [Real World OCaml](https://dev.realworldocaml.org/toc.html): A comprehensive guide to OCaml, covering practical examples and in-depth concepts.
- [OCaml Programming: Correct + Efficient + Beautiful](https://www.cs.cornell.edu/courses/cs3110/2022sp/): A course that covers OCaml programming, including functional programming, data structures, and algorithms.
- [Introduction to Functional Programming in OCaml](https://delimited-continuation.github.io/YSC1212/2022-2023_Sem2/index.html): Offers a fundamental understanding of functional programming principles using OCaml.
- [Introduction to Data Structures and Algorithms in OCaml](https://ilyasergey.net/YSC2229/): Offers a comprehensive understanding of data structures and algorithms using OCaml.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for more details.
