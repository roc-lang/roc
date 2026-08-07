# macOS Code Signing and Notarization

The nightly release workflow lives in
[`roc-lang/nightlies`](https://github.com/roc-lang/nightlies/blob/main/.github/workflows/nightly_new_compiler_all_os.yml).
Its macOS jobs need to code sign and notarize the Roc binary before packaging it
so downloaded nightlies run without Gatekeeper warnings.

The Apple Developer Program membership must be enrolled as an organization so
the signing identity belongs to the Roc Programming Language Foundation rather
than an individual. Apple requires the organization to be a legal entity with a
D-U-N-S Number, a public website, and a work email address on the organization's
domain. The person enrolling becomes the Account Holder and must have authority
to bind the organization to Apple's agreements.

## Required GitHub Secrets

Configure the following seven secrets in the
[`roc-lang/nightlies` Actions secrets](https://github.com/roc-lang/nightlies/settings/secrets/actions),
not in `roc-lang/roc`:

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

Enroll the foundation as an organization in the Apple Developer Program. The
program costs $99/year, although eligible nonprofit organizations can request a
fee waiver during enrollment.

### 2. Create Developer ID Certificate

1. In Keychain Access, choose Certificate Assistant > Request a Certificate
   from a Certificate Authority and save the certificate signing request to
   disk.
2. Go to [Certificates, Identifiers & Profiles](https://developer.apple.com/account/resources/certificates/list)
   and create a **Developer ID Application** certificate. Apple requires the
   Account Holder role for this certificate type.
3. Upload the certificate signing request, download the certificate, and open
   it to install it in Keychain Access.
4. In Keychain Access > My Certificates, export the Developer ID Application
   certificate and its private key together as a password-protected `.p12`.
5. Base64-encode the certificate:
   ```sh
   base64 -i certificate.p12 -o certificate-base64.txt
   ```
6. Use the contents of `certificate-base64.txt` as `MACOS_CERTIFICATE`

### 3. Create Notarization API Key

1. Go to [App Store Connect](https://appstoreconnect.apple.com) > Users and
   Access > Integrations > App Store Connect API.
2. If API access has not been enabled, the Account Holder must click **Request
   Access** and accept the terms.
3. Under **Team Keys**, create a key named `Roc Nightly Notarization` with
   **Developer** access. Do not create an Individual Key; individual keys
   cannot use `notarytool`.
4. Download the `.p8` key file. It can only be downloaded once, so also store a
   recovery copy in the foundation's secrets manager.
5. Note the Key ID and Issuer ID shown on the page.
6. Use these values for the `APPLE_NOTARIZATION_*` secrets.

### 4. Add Secrets to GitHub

Go to the `roc-lang/nightlies` repository's Settings > Secrets and variables >
Actions and add all seven secrets.

## How It Works

When the signing steps are enabled in `roc-lang/nightlies`:

1. After building the `roc` binary, CI imports the certificate into a
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

- [Apple Developer Program enrollment](https://developer.apple.com/help/account/membership/program-enrollment/)
- [Apple Developer Program fee waivers](https://developer.apple.com/help/account/membership/fee-waivers)
- [Developer ID certificates](https://developer.apple.com/help/account/certificates/create-developer-id-certificates/)
- [App Store Connect API keys](https://developer.apple.com/help/app-store-connect/get-started/app-store-connect-api/)
- [Customizing the notarization workflow](https://developer.apple.com/documentation/security/customizing-the-notarization-workflow)
