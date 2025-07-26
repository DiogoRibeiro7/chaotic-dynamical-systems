# Contributing Guidelines

We welcome contributions! Please keep commits focused and well described.
Use the following format for commit messages:

```
Short summary (50 characters or less)

Optional longer description explaining the motivation and approach.
```

Run the unit tests before submitting a pull request:

```bash
R -q -e 'devtools::load_all(); testthat::test_dir("tests/testthat")'
pytest -q
```
