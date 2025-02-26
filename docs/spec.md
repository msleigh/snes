## Documentation Overhaul Specification

### 1. **Objective:**
   Extend and improve the documentation in the repository by replacing Doxygen with Ford, while enhancing readability, structure, and organization. The documentation will be split into user, theory, and developer sections and delivered through Ford with a clean, Material-inspired theme.

---

### 2. **Documentation Structure:**
   The documentation will be organized into three main sections:

   - **User Documentation**
     - **Purpose**: Help users install, use, and run tests on the project.
     - **Contents**:
       - Installation instructions (dependencies, environment setup, etc.)
       - Usage guides (how to run the code, example use cases)
       - Test instructions (how to run the test suite, expected output, etc.)

   - **Theory Documentation**
     - **Purpose**: Provide background theory (maths/physics) behind the project.
     - **Contents**:
       - Explanation of the theory (formulas, physics concepts, etc.)
       - Diagrams (if necessary)
       - Any references or citations

   - **Developer Documentation**
     - **Purpose**: Guide developers on how to contribute, extend the codebase, and maintain the project.
     - **Contents**:
       - Code architecture (overview of modules, classes, functions)
       - Contribution guide (how to submit pull requests, coding standards)
       - Testing guide (how to add or modify tests, structure of tests, running tests)
       - Extending the code (how to add new features, best practices)

---

### 3. **Technology Stack:**
   - **Documentation Generator**: Ford (for generating all sections)
   - **Theme Inspiration**: The Material theme from MkDocs will serve as a reference for creating a clean, user-friendly layout within Ford.
   - **Documentation Format**: Markdown for all documentation files
   - **Automated Deployment**: GitHub Actions for continuous deployment to GitHub Pages

---

### 4. **Documentation Delivery:**
   - All documentation will be stored in the repository in Markdown format.
   - **Local Development**: Documentation can be viewed locally by running the Ford-generated site. The layout and structure will be designed to mimic the simplicity and readability of the MkDocs Material theme.
   - **Automated Hosting**: The site will be hosted on GitHub Pages and deployed automatically via GitHub Actions.
     - On every push to the main branch, the documentation will be built.
     - Developers can also trigger a manual rebuild/deployment if needed.

---

### 5. **Integration of Ford and MkDocs Material Theme:**
   - **MkDocs Integration**: While we won’t be using MkDocs itself, the **Material theme from MkDocs** will serve as a reference for Ford’s theme, focusing on simplicity, clarity, and readability.
   - **Ford Setup**:
     - Ford will handle the overall structure and generate the documentation.
     - We will customize Ford to achieve a clean, Material-like theme, with a simple layout focusing on readability.
     - Ford will be configured to extract Doxygen-style comments from the Fortran code to auto-generate the developer section.
   - **Customization**: The theme will be kept clean, without heavy custom styling, but will mimic Material’s structure and typography for consistency and readability.

---

### 6. **Ford Configuration for Doxygen-style Comments:**
   - **Objective**: Ensure Ford is set up to parse the Doxygen-style comments from the Fortran code (in-line with existing commenting format).
   - **Ford Configuration**:
     - Adjust settings in `ford.yaml` to handle Doxygen-style comments.
     - Ensure that the Fortran code files are parsed correctly, extracting the necessary API reference information.
     - Focus on auto-generating developer documentation (e.g., API references) from the Doxygen-style comments in the Fortran code.

---

### 7. **Version Control & Workflow:**
   - **GitHub Actions** will automate the process of generating and deploying the documentation to GitHub Pages.
   - **Process**:
     - On each push to the main branch:
       - Build documentation using Ford.
       - Deploy documentation to GitHub Pages.
     - **Manual Trigger**: Developers can manually trigger a build and deployment using GitHub Actions when they need to regenerate the documentation (e.g., after significant changes).

   - **Branching**:
     - The documentation changes will be made directly on the main branch, keeping the workflow simple.

---

### 8. **Error Handling Strategy:**
   - If the build fails (either locally or during deployment), clear and actionable error messages will be provided in the build logs (via GitHub Actions).
   - **Common Issues**:
     - Missing or misconfigured settings in `ford.yaml`.
     - Incorrect Doxygen-style comments that aren't parsed correctly.
     - Build failures due to missing files or invalid Markdown syntax.
   - **Plan**: Troubleshoot build failures by checking logs, confirming the configuration, and fixing issues within the relevant markdown files or configuration.

---

### 9. **Testing Plan:**
   - **Documentation Testing**:
     - Ensure that all sections (user, theory, developer) are included in the documentation.
     - Check that the Doxygen-style comments are correctly parsed by Ford.
     - Verify that the content is clearly structured, easy to read, and follows the established hierarchy.
     - Ensure that links, images, and code examples render properly.

   - **Automated Tests**:
     - Make sure that the documentation build and deployment are fully automated via GitHub Actions, and test the deployment on GitHub Pages.

   - **Manual Review**:
     - Manual review of the documentation should be conducted after each major update to ensure accuracy, completeness, and clarity.

---

### 10. **Additional Developer Considerations:**
   - **Contribution**: Document how other developers can contribute to the documentation (e.g., submitting PRs for corrections or updates to specific sections).
   - **Future Extensions**: If the project evolves (e.g., more sections are added), provide guidelines for how developers can extend the documentation, including how to add new sections or update the existing ones.

---

## Next Steps for Developer:
1. **Set Up Ford**: Install and configure Ford to parse the existing Doxygen-style comments.
2. **Set Up Theme**: Customize the Ford theme to mimic the Material theme from MkDocs, ensuring simplicity and readability.
3. **Create `ford.yaml`**: Configure Ford to pull Doxygen-style comments and ensure the content is structured properly.
4. **GitHub Actions Workflow**: Set up automated deployment with GitHub Actions for building and hosting the documentation on GitHub Pages.
5. **Test the Process**: Run through the process of building the documentation locally and verifying its appearance on GitHub Pages.
