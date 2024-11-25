;; land-registry-core.clar

;; Error codes
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_INVALID_INPUT (err u102))
(define-constant ERR_ALREADY_EXISTS (err u103))
(define-constant ERR_INVALID_STATUS (err u104))

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ACTIVE_STATUS "active")
(define-constant LOCKED_STATUS "locked")

;; Data Maps
(define-map properties
    { property-id: uint }
    {
        owner: principal,
        location: (string-ascii 100),
        size: uint,
        status: (string-ascii 20),
        last-price: uint,
        registration-date: uint
    }
)

(define-map property-documents
    { property-id: uint, doc-id: uint }
    {
        doc-hash: (string-ascii 64),
        doc-type: (string-ascii 20),
        timestamp: uint
    }
)

(define-map property-counters
    { counter-type: (string-ascii 20) }
    { value: uint }
)

;; Private Functions
(define-private (get-next-id (counter-type (string-ascii 20)))
    (let ((current-value (default-to { value: u0 } (map-get? property-counters { counter-type: counter-type }))))
        (begin
            (map-set property-counters
                { counter-type: counter-type }
                { value: (+ (get value current-value) u1) })
            (+ (get value current-value) u1))))

(define-private (validate-property-status (status (string-ascii 20)))
    (or
        (is-eq status ACTIVE_STATUS)
        (is-eq status LOCKED_STATUS)))

;; Public Functions
(define-public (register-property 
    (location (string-ascii 100))
    (size uint)
    (initial-price uint))
    (let
        ((property-id (get-next-id "property"))
         (current-time (unwrap! (get-block-info? time u0) ERR_INVALID_INPUT)))
        (begin
            (asserts! (> size u0) ERR_INVALID_INPUT)
            (asserts! (> initial-price u0) ERR_INVALID_INPUT)
            (map-set properties
                { property-id: property-id }
                {
                    owner: tx-sender,
                    location: location,
                    size: size,
                    status: ACTIVE_STATUS,
                    last-price: initial-price,
                    registration-date: current-time
                })
            (print {
                event: "property-registered",
                property-id: property-id,
                owner: tx-sender
            })
            (ok property-id))))

(define-public (transfer-property
    (property-id uint)
    (new-owner principal))
    (let ((property (unwrap! (map-get? properties { property-id: property-id }) ERR_NOT_FOUND)))
        (begin
            (asserts! (is-eq tx-sender (get owner property)) ERR_NOT_AUTHORIZED)
            (asserts! (is-eq (get status property) ACTIVE_STATUS) ERR_INVALID_STATUS)
            (map-set properties
                { property-id: property-id }
                (merge property { owner: new-owner }))
            (print {
                event: "property-transferred",
                property-id: property-id,
                previous-owner: tx-sender,
                new-owner: new-owner
            })
            (ok true))))

(define-read-only (get-property-details (property-id uint))
    (ok (unwrap! (map-get? properties { property-id: property-id }) ERR_NOT_FOUND)))

(define-public (add-property-document
    (property-id uint)
    (doc-hash (string-ascii 64))
    (doc-type (string-ascii 20)))
    (let
        ((property (unwrap! (map-get? properties { property-id: property-id }) ERR_NOT_FOUND))
         (doc-id (get-next-id "document"))
         (current-time (unwrap! (get-block-info? time u0) ERR_INVALID_INPUT)))
        (begin
            (asserts! (is-eq tx-sender (get owner property)) ERR_NOT_AUTHORIZED)
            (map-set property-documents
                { property-id: property-id, doc-id: doc-id }
                {
                    doc-hash: doc-hash,
                    doc-type: doc-type,
                    timestamp: current-time
                })
            (print {
                event: "document-added",
                property-id: property-id,
                doc-id: doc-id
            })
            (ok doc-id))))

;; property-token.clar

(impl-trait 'SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait.nft-trait)

(define-non-fungible-token property-token uint)

(define-map token-uris
    { token-id: uint }
    { uri: (string-ascii 256) }
)

(define-map token-count principal uint)

(define-public (mint-property-token (token-id uint) (recipient principal) (token-uri (string-ascii 256)))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
        (try! (nft-mint? property-token token-id recipient))
        (map-set token-uris
            { token-id: token-id }
            { uri: token-uri })
        (ok true)))

(define-public (transfer-token (token-id uint) (sender principal) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender sender) ERR_NOT_AUTHORIZED)
        (nft-transfer? property-token token-id sender recipient)))

(define-read-only (get-token-uri (token-id uint))
    (ok (get uri (unwrap! (map-get? token-uris { token-id: token-id }) ERR_NOT_FOUND))))

(define-read-only (get-owner (token-id uint))
    (ok (nft-get-owner? property-token token-id)))

;; registry-governance.clar

(define-data-var contract-admin principal CONTRACT_OWNER)
(define-data-var contract-paused bool false)

(define-public (set-admin (new-admin principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-admin)) ERR_NOT_AUTHORIZED)
        (var-set contract-admin new-admin)
        (ok true)))

(define-public (pause-contract)
    (begin
        (asserts! (is-eq tx-sender (var-get contract-admin)) ERR_NOT_AUTHORIZED)
        (var-set contract-paused true)
        (ok true)))

(define-public (resume-contract)
    (begin
        (asserts! (is-eq tx-sender (var-get contract-admin)) ERR_NOT_AUTHORIZED)
        (var-set contract-paused false)
        (ok true)))

(define-private (is-contract-admin)
    (is-eq tx-sender (var-get contract-admin)))

;; escrow.clar

(define-map escrows
    { escrow-id: uint }
    {
        property-id: uint,
        seller: principal,
        buyer: principal,
        amount: uint,
        status: (string-ascii 20),
        created-at: uint
    }
)

(define-constant ESCROW_ACTIVE "active")
(define-constant ESCROW_COMPLETED "completed")
(define-constant ESCROW_CANCELLED "cancelled")

(define-public (create-escrow (property-id uint) (buyer principal) (amount uint))
    (let
        ((escrow-id (get-next-id "escrow"))
         (property (unwrap! (map-get? properties { property-id: property-id }) ERR_NOT_FOUND))
         (current-time (unwrap! (get-block-info? time u0) ERR_INVALID_INPUT)))
        (begin
            (asserts! (is-eq (get owner property) tx-sender) ERR_NOT_AUTHORIZED)
            (map-set escrows
                { escrow-id: escrow-id }
                {
                    property-id: property-id,
                    seller: tx-sender,
                    buyer: buyer,
                    amount: amount,
                    status: ESCROW_ACTIVE,
                    created-at: current-time
                })
            (print {
                event: "escrow-created",
                escrow-id: escrow-id,
                property-id: property-id
            })
            (ok escrow-id))))

(define-public (complete-escrow (escrow-id uint))
    (let
        ((escrow (unwrap! (map-get? escrows { escrow-id: escrow-id }) ERR_NOT_FOUND))
         (property (unwrap! (map-get? properties { property-id: (get property-id escrow) }) ERR_NOT_FOUND)))
        (begin
            (asserts! (is-eq (get status escrow) ESCROW_ACTIVE) ERR_INVALID_STATUS)
            (asserts! (or (is-eq tx-sender (get buyer escrow)) (is-eq tx-sender (get seller escrow))) ERR_NOT_AUTHORIZED)
            (try! (stx-transfer? (get amount escrow) (get buyer escrow) (get seller escrow)))
            (try! (transfer-property (get property-id escrow) (get buyer escrow)))
            (map-set escrows
                { escrow-id: escrow-id }
                (merge escrow { status: ESCROW_COMPLETED }))
            (print {
                event: "escrow-completed",
                escrow-id: escrow-id
            })
            (ok true))))