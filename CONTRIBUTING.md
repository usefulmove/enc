## Contributing to `enc`

Thank you for your interest in contributing to `enc`, the Emacs buffer and region encryption tool. This document outlines the process for contributing to this project. Following these guidelines helps ensure a smooth collaboration experience for everyone involved.

### Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
  - [Setting Up for Local Development](#setting-up-for-local-development)
- [How to Contribute](#how-to-contribute)
  - [Reporting Bugs](#reporting-bugs)
  - [Suggesting Enhancements](#suggesting-enhancements)
  - [Your First Code Contribution](#your-first-code-contribution)
- [Pull Requests](#pull-request-process)
- [Style Guides](#styleguides)
  - [Git Commit Messages](#git-commit-messages)
  - [Style Guide](#elisp-styleguide)
- [Acknowledgements](#acknowledgements)

## Code of Conduct

This project and everyone participating in it are governed by the `enc` Code of Conduct. By participating, you are expected to uphold this code. Please report unacceptable behavior.

## Getting Started

### Setting Up for Local Development

1. Fork the `enc` repository on GitHub.
2. Clone your fork of the repository.
   ```bash
   git clone https://github.com/<your-username>/enc.git
   ```

3. Add the original repo as an upstream remote.
   ```bash
   git remote add upstream https://github.com/usefulmove/enc.git
   ```

4. Create a new branch for your work.
   ```bash
   git checkout -b <branch-name>
   ```

5. Make your changes and commit them.
6. Push your branch to your fork.
7. Create a pull request from your branch to the original repository.

## How to Contribute

### Reporting Bugs

1. Ensure the bug was not already reported by searching on GitHub under [Issues](https://github.com/usefulmove/enc/issues).
2. If you're unable to find an open issue addressing the problem, [open a new one](https://github.com/usefulmove/enc/issues/new). Include a clear title and description, and as much relevant information as possible.

### Suggesting Enhancements

1. First, check if there's already an issue for the enhancement you have in mind.
2. If no such issue exists, [open a new one](https://github.com/usefulmove/enc/issues/new). Be sure to include a clear title, a detailed description, and the reason or use case for the enhancement.

### Your First Code Contribution

- Unsure where to begin contributing? Start by looking at `Good First Issue` tags in our issues.

## Pull Request Process

1. Ensure that your code adheres to our coding guidelines and style guides.
2. Update the README.md with details of changes if necessary.
3. Increase the version numbers in any examples files and the README.md to the new version that this Pull Request would represent.
4. Your Pull Request will be reviewed by the maintainers. Address any comments and make necessary changes.
5. Once approved, your Pull Request will be merged by a maintainer.

## Styleguides

### Git Commit Messages

- Use the present tense ("Add feature" not "Added feature").
- Limit the first line to 72 characters or less.
- Reference issues and pull requests liberally.

### Elisp Styleguide

- Use Emacs' built-in Elisp conventions.
- Indent with spaces, not tabs.
- Keep functions and variables properly documented.
- Write self-explanatory code and add comments where necessary.

## Acknowledgements

A big thank you to all our contributors and users. Your feedback, contributions, and engagement keep the project alive and thriving.

---

Thank you for all of your help!
