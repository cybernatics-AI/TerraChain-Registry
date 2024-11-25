# Terrahub: Blockchain Land Registry System

## Overview

Terrahub is a smart contract on the Stacks blockchain for secure and efficient land registry and property management.

## Key Features

1. **Property Registration & Transfer**: Register and transfer property ownership.
2. **NFT Implementation**: Properties as unique tokens.
3. **Document Management**: Add and retrieve property documents.
4. **Escrow System**: Secure property transactions.
5. **Administrative Controls**: Contract management functions.

## Main Functions

- `register-property`: Add new properties
- `transfer-property`: Change property ownership
- `add-property-document`: Attach documents to properties
- `create-escrow`, `complete-escrow`, `cancel-escrow`: Manage property sales
- `get-property-details`, `get-escrow-details`: Retrieve information

## Error Handling

Includes specific error codes (e.g., ERR_NOT_AUTHORIZED, ERR_NOT_FOUND) for various scenarios.

## Data Storage

Uses data maps for properties, documents, escrows, and NFT token URIs.

## Usage

1. Deploy to Stacks blockchain
2. Interact via Stacks wallet or API calls

## Security

- Access control for sensitive operations
- Pausable contract for emergencies

## Future Plans

- Dispute resolution for escrows
- Fractional property ownership
- Integration with property valuation oracles

## Contributing

Open to contributions. Submit pull requests or issues for proposed changes.
