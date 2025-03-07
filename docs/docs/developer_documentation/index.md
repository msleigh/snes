---
title: Developer Documentation
ordered_subpage: spec.md
ordered_subpage: prompt_plan.md
---

This section provides information for developers on how to contribute to the project and understand the codebase.

## Code Structure

The codebase is organized into several key directories and files:

- `src/`: Contains the Fortran source code files for the main application logic, including modules for data handling, computation, and I/O operations.
- `qa/`: Includes input files for test problems and scripts for running and verifying tests.
- `nucdata/`: Stores nuclear data files used by the application for simulations.
- `docs/`: Contains documentation files, including user and developer guides, and configuration for building the documentation.
- `Makefile`: Defines build rules for compiling the application and running tests.
- `Dockerfile`: Provides instructions for building a Docker image to run the application in a containerized environment.

## Contribution Guidelines

To contribute to the project, follow these steps to set up your development environment:

1. **Clone the Repository**: Start by cloning the repository to your local machine.

   ```bash
   git clone <repository-url>
   cd <repository-directory>
   ```

2. **Install Pre-commit Hooks**: This project uses pre-commit hooks to ensure code quality. Install the pre-commit package and set up the hooks by running:

   ```bash
   pip install pre-commit
   pre-commit install
   ```

   This will automatically run checks on your code before each commit, helping maintain code quality and consistency.
