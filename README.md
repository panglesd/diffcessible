---

# Diffcessible: An accessible diff viewer

Diffcessible is an terminal based diff viewer with an emphasis on being accessible. Our goal is to build a diff viewer on top of Git diff that is accessible to screen readers.

## Overview

Diffcessible currently has two main components:

1. `bin/main.ml`: The main entry point for the application. It is responsible for parsing the command-line arguments and calling the appropriate functions to display the diff. The key point to note here is that the command-line requires a path to a diff file as a required positional argument. See the section on [Running the Project](#running-the-Project) for more details.

2. `lib/interactive_viewer.ml`: The main logic for the diff viewer. It is responsible for parsing the diff and displaying it in the terminal. This fetches the hunk information from the diff and displays it in a user-friendly manner by indexing the hunks. The core logic for the viewer is implemented in the `view` function, which allows the user to navigate through the hunks using the `n` and `p` keys to move to the next and previous hunks, respectively. The `q` key can be used to quit the viewer.

## Installation

To set up the project, please fork the repository and clone it to your local machine. You will need to have `opam` installed to install the dependencies. Once you have `opam` installed, you can run the following command to install the dependencies:

```bash
opam install . --deps-only --with-test
```

## Running the Project

To run the project, you can use the following commands:

```bash
dune exec diffcessible [path/to/file1]
```

## Roadmap

As this project is in its early stages, we have a few features that we would like to implement:

- [ ] Side-by-side diff viewer
- [ ] Color coding for added, removed, and modified lines
- [ ] Display metadata about the diff and more information on the status bar
- [ ] Binding `j` and `k` for scrolling

## Technologies Used

The following is a list of the technologies used in the project:

- **Patch Library:** For parsing and printing diffs.
- **Notty & Lwd Libraries:** For terminal layout declaration and creating a reactive UI.
- **Nottui Library:** Integrates Lwd and Notty for UI development.
- **Cmdliner Library:** Facilitates command-line interface creation.
- **Dune Build System:** For project building.

## Contributing

Contributions are welcome! Please refer to the [CONTRIBUTING.md](CONTRIBUTING.md) file for detailed instructions on how to contribute to this project.

## Learning Resources

For those new to OCaml or looking to enhance their skills, here are some recommended resources:

- [Real World OCaml](https://dev.realworldocaml.org/toc.html): A comprehensive guide to OCaml, covering practical examples and in-depth concepts.
- [Introduction to Functional Programming in OCaml](https://delimited-continuation.github.io/YSC1212/2022-2023_Sem2/index.html): Offers a fundamental understanding of functional programming principles using OCaml.
- [Practical Foundations for Programming Languages in OCaml](https://ilyasergey.net/YSC2229/): Offers a comprehensive understanding of data structures and algorithms using OCaml.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for more details.

---
