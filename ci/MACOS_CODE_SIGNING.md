# macOS Code Signing and Notarization

The nightly builds for the new Zig-based compiler are code signed and notarized
for macOS. This allows users to download and run the binary without Gatekeeper
warnings.

## Required GitHub Secrets

The following secrets must be configured in the repository settings:

### Code Signing Secrets

| Secret | Description |
|--------|-------------|
| `MACOS_CERTIFICATE` | Base64-encoded `.p12` Developer ID Application certificate |
| `MACOS_CERTIFICATE_PWD` | Password for the `.p12` certificate file |
| `MACOS_CERTIFICATE_NAME` | Certificate identity name (e.g., `Developer ID Application: Roc Programming Language Foundation (XXXXXXXXXX)`) |
| `MACOS_CI_KEYCHAIN_PWD` | Password for the temporary CI keychain (can be any secure random string) |

### Notarization Secrets

| Secret | Description |
|--------|-------------|
| `APPLE_NOTARIZATION_KEY_ID` | Key ID from App Store Connect API Keys |
| `APPLE_NOTARIZATION_ISSUER` | Issuer UUID from App Store Connect API Keys |
| `APPLE_NOTARIZATION_KEY` | Private key content in `.p8` format from App Store Connect |

## Setup Instructions

### 1. Apple Developer Program

You need an Apple Developer Program membership ($99/year) to get a Developer ID
certificate and access notarization services.

### 2. Create Developer ID Certificate

1. Go to [Apple Developer](https://developer.apple.com) > Certificates, Identifiers & Profiles
2. Create a new "Developer ID Application" certificate
3. Download and install in Keychain Access
4. Export as `.p12` with a password
5. Base64-encode the certificate:
   ```sh
   base64 -i certificate.p12 -o certificate-base64.txt
   ```
6. Use the contents of `certificate-base64.txt` as `MACOS_CERTIFICATE`

### 3. Create Notarization API Key

1. Go to [App Store Connect](https://appstoreconnect.apple.com) > Users and Access > Integrations > App Store Connect API
2. Create a new key with "Developer" access
3. Download the `.p8` key file (can only be downloaded once!)
4. Note the Key ID and Issuer ID shown on the page
5. Use these values for the `APPLE_NOTARIZATION_*` secrets

### 4. Add Secrets to GitHub

Go to Repository Settings > Secrets and variables > Actions and add all six secrets.

## How It Works

1. After building the `roc` binary, the CI imports the certificate into a
   temporary keychain
2. The binary is signed with `codesign` using the Developer ID certificate and
   hardened runtime (`-o runtime`)
3. The signed binary is zipped and submitted to Apple's notarization service
4. Apple scans the binary and, if approved, records it in their database
5. When users download and run the binary, macOS checks Apple's servers to
   verify it's notarized

## Troubleshooting

### "The executable does not have the hardened runtime enabled"

Make sure `-o runtime` is passed to `codesign`.

### Notarization rejected

Check the notarization log:
```sh
xcrun notarytool log <submission-id> --keychain-profile "notarytool-profile"
```

### Certificate not found

Verify the `MACOS_CERTIFICATE_NAME` matches exactly what's shown in Keychain
Access for the certificate's "Common Name".

## References

- [Notarize a Command Line Tool with notarytool](https://scriptingosx.com/2021/07/notarize-a-command-line-tool-with-notarytool/)
- [Apple Developer Documentation: Customizing the notarization workflow](https://developer.apple.com/documentation/security/customizing-the-notarization-workflow)
