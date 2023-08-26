# enc . emacs buffer and region encryption

**enc** provides simple buffer and region encryption and decryption for [GNU Emacs](https://www.gnu.org/software/emacs/).

**Warning**:  Please note that the encryption method used by **enc** is not designed or intended for high-importance, high-security, or critical applications. This includes, but is not limited to, the protection of sensitive personal data, financial transactions, national security information, or any other data where unauthorized access could have severe consequences.

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

Made by [usefulmove](https://github.com/usefulmove). 🛡️
