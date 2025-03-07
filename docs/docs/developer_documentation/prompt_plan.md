---
title: Prompt plan
---

## Blueprint

### 1. Environment Setup and Preliminary Work
- **Objective:** Prepare the local environment and verify existing assets.
- **Steps:**
  1. Create a new branch for the documentation overhaul.
  2. Install Ford locally and verify that the installation works.
  3. Ensure that the Fortran source files (with Doxygen-style comments) are accessible.
  4. Write a simple test (or script) that runs Ford to generate a basic documentation output.

### 2. Ford Configuration
- **Objective:** Replace the Doxygen-based setup with Ford.
- **Steps:**
  1. Create or update a `ford.yaml` configuration file.
  2. Configure Ford to parse Fortran code and extract Doxygen-style comments.
  3. Ensure the output is in Markdown and follows the desired documentation structure.

### 3. Custom Ford Theme Development (Inspired by MkDocs Material)
- **Objective:** Develop a custom Ford theme that mimics the clean, user-friendly layout of the MkDocs Material theme.
- **Steps:**
  1. Analyze the MkDocs Material theme to identify key style elements (fonts, layout, colors).
  2. Develop a minimal Ford theme that incorporates these elements.
  3. Integrate the theme into the Ford configuration and test the visual output.
  4. Iterate on the theme to improve readability and navigation.

### 4. Documentation Structure Implementation
- **Objective:** Organize documentation into three distinct sections.
- **Steps:**
  1. Create placeholder Markdown files for:
     - **User Documentation:** Installation, usage, testing.
     - **Theory Documentation:** Maths/physics explanations.
     - **Developer Documentation:** Code architecture, contribution guidelines, API reference.
  2. Update the Ford configuration to include these sections in the generated documentation.
  3. Verify that each section is rendered correctly and is easily navigable.

### 5. GitHub Actions Integration
- **Objective:** Automate the build and deployment of documentation.
- **Steps:**
  1. Create a GitHub Actions workflow file.
  2. Configure the workflow to:
     - Trigger a build on every push to the main branch.
     - Allow manual dispatch of the workflow.
     - Build the documentation using Ford.
     - Deploy the generated documentation to GitHub Pages.
  3. Test the workflow incrementally, ensuring error handling and logging are robust.

### 6. End-to-End Integration Testing and Finalization
- **Objective:** Wire all components together and ensure the entire flow works as expected.
- **Steps:**
  1. Perform local end-to-end tests of the documentation build process.
  2. Push changes to trigger GitHub Actions and verify deployment on GitHub Pages.
  3. Write and run integration tests that check for:
     - Presence of all documentation sections.
     - Correct parsing of Fortran Doxygen comments.
     - Successful deployment without errors.
  4. Finalize the setup and document any troubleshooting steps.

---

## Iterative Breakdown into Small Chunks

### Iteration 1: Environment Setup and Preliminary Work
- **Step 1.1:** Create a new Git branch for the documentation project.
- **Step 1.2:** Install Ford locally (ensure prerequisites are met).
- **Step 1.3:** Verify access to Fortran files with Doxygen comments.
- **Step 1.4:** Write and run a simple test script to generate basic documentation with Ford.

### Iteration 2: Ford Configuration
- **Step 2.1:** Create a minimal `ford.yaml` configuration file.
- **Step 2.2:** Configure Ford to process Fortran files and extract Doxygen comments.
- **Step 2.3:** Generate a sample documentation output and validate that developer docs are included.
- **Step 2.4:** Adjust settings as needed based on test output.

### Iteration 3: Custom Ford Theme Development
- **Step 3.1:** Research and document key style elements from the MkDocs Material theme.
- **Step 3.2:** Create a basic custom theme for Ford incorporating these elements.
- **Step 3.3:** Integrate the custom theme into the Ford configuration.
- **Step 3.4:** Generate documentation to verify the theme’s layout and readability.

### Iteration 4: Documentation Structure Implementation
- **Step 4.1:** Create placeholder Markdown files for the User, Theory, and Developer sections.
- **Step 4.2:** Update the Ford configuration to include these documentation sections.
- **Step 4.3:** Build documentation and test that each section appears and is properly navigable.
- **Step 4.4:** Write unit tests (or manual checklists) to confirm the structure.

### Iteration 5: GitHub Actions Workflow Integration
- **Step 5.1:** Draft a basic GitHub Actions workflow file for documentation build.
- **Step 5.2:** Configure the workflow to trigger on every push to main and allow manual dispatch.
- **Step 5.3:** Test the workflow locally by simulating a push event.
- **Step 5.4:** Verify deployment to a GitHub Pages test branch.

### Iteration 6: End-to-End Integration Testing and Finalization
- **Step 6.1:** Run a full local build of the documentation.
- **Step 6.2:** Push changes to the main branch and monitor the GitHub Actions logs.
- **Step 6.3:** Validate that the documentation is deployed correctly on GitHub Pages.
- **Step 6.4:** Perform integration tests and document troubleshooting procedures.
- **Step 6.5:** Wire all components together and finalize the project.

---

## Series of Prompts for a Code-Generation LLM

Below are the prompts (each in its own code block) that you can use with a code-generation LLM. Each prompt is designed to be small, incremental, and test-driven.

### Prompt 1: Environment Setup and Preliminary Work
```text
# Prompt 1: Environment Setup and Preliminary Work

We are on a Git branch called 'feature/update-docs', for the documentation project. Ensure that Ford is installed locally. We use the UV tool for managing Python tools and dependencies, with a 'pyproject.yaml' file. Write a small script (or set of instructions) that verifies Ford's installation by generating a basic documentation output from a simple Fortran file that contains a Doxygen-style comment.

Ensure that all dependencies are correctly installed and the environment is configured to support the entire documentation workflow. Remember to take small, incremental steps and test each part before proceeding.
```

### Prompt 2: Create and Configure `ford.yaml`
```text
# Prompt 2: Create and Configure `ford.yaml`

Create a comprehensive `ford.yaml` configuration file that instructs Ford to parse Fortran files and extract Doxygen-style comments. Ensure the configuration supports the entire project structure and includes all necessary files. Start with a basic configuration and include a simple test case: a Fortran file with a Doxygen comment that should be included in the generated documentation.

Break the work into small steps and verify the output before adding more complexity. Ensure the configuration aligns with the project's overall documentation strategy.
```

### Prompt 3: Develop Custom Ford Theme Inspired by MkDocs Material
```text
# Prompt 3: Develop Custom Ford Theme Inspired by MkDocs Material

Develop a comprehensive custom Ford theme that takes inspiration from the MkDocs Material theme. Begin by identifying key style elements (such as fonts, layout, and color scheme) from MkDocs Material. Then, create a detailed theme configuration file for Ford that applies these styles.

Ensure that the theme is integrated into the Ford configuration and generate documentation to verify the visual layout. Ensure the theme supports the entire documentation structure and enhances readability and navigation. Please take small, incremental steps and test the theme integration at each stage.
```

### Prompt 4: Documentation Structure Implementation
```text
# Prompt 4: Documentation Structure Implementation

Organize the documentation into three sections: User Documentation, Theory Documentation, and Developer Documentation. Create placeholder Markdown files for each section. Update the Ford configuration so that these sections are included in the generated documentation.

Verify that each section appears correctly and is navigable. Ensure the documentation structure aligns with the project's overall goals and provides clear guidance for users, developers, and theorists. Provide small code samples and test that the sections are correctly linked. Proceed in small steps with frequent testing.
```

### Prompt 5: GitHub Actions Workflow for Documentation Build and Deployment
```text
# Prompt 5: GitHub Actions Workflow for Documentation Build and Deployment

Set up a GitHub Actions workflow file that triggers a documentation build and deployment on every push to the main branch, and also supports manual dispatch. The workflow should run Ford to generate the documentation and deploy it to GitHub Pages.

Ensure the workflow is robust and supports the entire documentation lifecycle, including error handling and logging. Begin by creating a basic workflow file, then incrementally add steps to build and deploy the documentation. Test each part of the workflow thoroughly before moving on.
```

### Prompt 6: End-to-End Integration Testing and Finalization
```text
# Prompt 6: End-to-End Integration Testing and Finalization

Integrate all the components by performing an end-to-end test. Start by running a complete local build of the documentation. Then push the changes to the main branch to trigger the GitHub Actions workflow. Verify that the documentation is successfully deployed to GitHub Pages.

Write and run comprehensive integration tests that confirm:
- All documentation sections (User, Theory, Developer) are present.
- Doxygen-style comments from the Fortran code are correctly parsed.
- The GitHub Actions workflow completes successfully.

Ensure that the instructions include steps for reviewing logs and handling any errors. Ensure the entire documentation process is seamless and aligns with the project's overall objectives. End by wiring all components together.
```
