# enc . emacs buffer encryption . 

**enc** is an open source project that provides seamless integration for buffer and selected region encryption & decryption in emacs.

## Table of Contents

- [Installation](#installation)
- [Usage](#usage)
- [Contributing](#contributing)
- [License](#license)
- [Acknowledgements](#acknowledgements)


## Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/usefulmove/enc.git
   ```

2. Move `enc.el` to your `.emacs.d` directory or any directory in your `load-path`.

3. Add the following lines to your `.emacs` or `init.el` file:
   ```elisp
   (add-to-list 'load-path "/path/to/enc-directory")
   (require 'enc)
   ```

4. Restart Emacs to complete the installation.

## Usage

### Encrypt Buffer

To encrypt the entire buffer:

```elisp
M-x enc-encrypt-buffer
```

### Encrypt Selected Region

To encrypt a selected region:

```elisp
M-x enc-encrypt-region
```

### Decrypt Buffer

To decrypt the entire buffer:

```elisp
M-x enc-decrypt-buffer
```

### Decrypt Selected Region

To decrypt a selected region:

```elisp
M-x enc-decrypt-region
```

## Contributing

Contributions, bug reports, and feature requests are welcome! If you feel like contributing, please follow our [CONTRIBUTING.md](./CONTRIBUTING.md) guidelines.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgements

- Thanks to the Emacs community for constant inspiration and support.
- To all our contributors and users, thank you for making **enc** better.

---

Made with ❤️ by [your_name_or_team_name](https://github.com/your_username). Happy Encrypting! 🛡️
