;; Customer Journey Contract
;; Tracks cross-channel customer interactions and preferences

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u300))
(define-constant err-not-found (err u301))
(define-constant err-already-exists (err u302))
(define-constant err-invalid-input (err u303))
(define-constant err-unauthorized (err u304))

;; Data Variables
(define-data-var next-customer-id uint u1)
(define-data-var next-interaction-id uint u1)

;; Data Maps
(define-map customers
  { customer-id: uint }
  {
    wallet: principal,
    first-interaction: uint,
    last-interaction: uint,
    total-interactions: uint,
    preferred-channels: (list 5 (string-ascii 20)),
    loyalty-score: uint,
    lifetime-value: uint
  }
)

(define-map customer-by-wallet
  { wallet: principal }
  { customer-id: uint }
)

(define-map interactions
  { interaction-id: uint }
  {
    customer-id: uint,
    channel-id: uint,
    interaction-type: (string-ascii 30),
    timestamp: uint,
    data-hash: (string-ascii 64),
    satisfaction-score: (optional uint),
    conversion: bool
  }
)

(define-map customer-preferences
  { customer-id: uint }
  {
    communication-frequency: (string-ascii 20),
    preferred-categories: (list 10 (string-ascii 30)),
    price-sensitivity: uint,
    brand-loyalty: uint,
    last-updated: uint
  }
)

(define-map journey-analytics
  { customer-id: uint }
  {
    total-touchpoints: uint,
    conversion-rate: uint,
    average-session-duration: uint,
    most-active-channel: (string-ascii 20),
    churn-risk: uint,
    next-best-action: (string-ascii 100)
  }
)

;; Public Functions

;; Register a new customer
(define-public (register-customer (wallet principal))
  (let
    (
      (customer-id (var-get next-customer-id))
    )
    (asserts! (is-none (map-get? customer-by-wallet { wallet: wallet })) err-already-exists)

    ;; Create customer record
    (map-set customers
      { customer-id: customer-id }
      {
        wallet: wallet,
        first-interaction: block-height,
        last-interaction: block-height,
        total-interactions: u0,
        preferred-channels: (list),
        loyalty-score: u0,
        lifetime-value: u0
      }
    )

    ;; Map wallet to customer ID
    (map-set customer-by-wallet
      { wallet: wallet }
      { customer-id: customer-id }
    )

    ;; Initialize preferences
    (map-set customer-preferences
      { customer-id: customer-id }
      {
        communication-frequency: "weekly",
        preferred-categories: (list),
        price-sensitivity: u50,
        brand-loyalty: u50,
        last-updated: block-height
      }
    )

    ;; Initialize analytics
    (map-set journey-analytics
      { customer-id: customer-id }
      {
        total-touchpoints: u0,
        conversion-rate: u0,
        average-session-duration: u0,
        most-active-channel: "unknown",
        churn-risk: u20,
        next-best-action: "welcome-sequence"
      }
    )

    ;; Increment next ID
    (var-set next-customer-id (+ customer-id u1))

    (ok customer-id)
  )
)

;; Track customer interaction
(define-public (track-interaction
  (customer-id uint)
  (channel-id uint)
  (interaction-type (string-ascii 30))
  (data-hash (string-ascii 64))
  (conversion bool)
)
  (let
    (
      (interaction-id (var-get next-interaction-id))
      (customer (unwrap! (map-get? customers { customer-id: customer-id }) err-not-found))
    )
    (asserts! (> (len interaction-type) u0) err-invalid-input)

    ;; Record interaction
    (map-set interactions
      { interaction-id: interaction-id }
      {
        customer-id: customer-id,
        channel-id: channel-id,
        interaction-type: interaction-type,
        timestamp: block-height,
        data-hash: data-hash,
        satisfaction-score: none,
        conversion: conversion
      }
    )

    ;; Update customer record
    (map-set customers
      { customer-id: customer-id }
      (merge customer {
        last-interaction: block-height,
        total-interactions: (+ (get total-interactions customer) u1)
      })
    )

    ;; Increment next interaction ID
    (var-set next-interaction-id (+ interaction-id u1))

    (ok interaction-id)
  )
)

;; Update customer preferences
(define-public (update-preferences
  (customer-id uint)
  (frequency (string-ascii 20))
  (categories (list 10 (string-ascii 30)))
  (price-sensitivity uint)
  (brand-loyalty uint)
)
  (let
    (
      (preferences (unwrap! (map-get? customer-preferences { customer-id: customer-id }) err-not-found))
    )
    (asserts! (<= price-sensitivity u100) err-invalid-input)
    (asserts! (<= brand-loyalty u100) err-invalid-input)

    (map-set customer-preferences
      { customer-id: customer-id }
      {
        communication-frequency: frequency,
        preferred-categories: categories,
        price-sensitivity: price-sensitivity,
        brand-loyalty: brand-loyalty,
        last-updated: block-height
      }
    )

    (ok true)
  )
)

;; Record satisfaction score
(define-public (record-satisfaction (interaction-id uint) (score uint))
  (let
    (
      (interaction (unwrap! (map-get? interactions { interaction-id: interaction-id }) err-not-found))
    )
    (asserts! (<= score u100) err-invalid-input)

    (map-set interactions
      { interaction-id: interaction-id }
      (merge interaction { satisfaction-score: (some score) })
    )

    (ok score)
  )
)

;; Update loyalty score
(define-public (update-loyalty-score (customer-id uint) (new-score uint))
  (let
    (
      (customer (unwrap! (map-get? customers { customer-id: customer-id }) err-not-found))
    )
    (asserts! (<= new-score u100) err-invalid-input)

    (map-set customers
      { customer-id: customer-id }
      (merge customer { loyalty-score: new-score })
    )

    (ok new-score)
  )
)

;; Update lifetime value
(define-public (update-lifetime-value (customer-id uint) (value uint))
  (let
    (
      (customer (unwrap! (map-get? customers { customer-id: customer-id }) err-not-found))
    )
    (map-set customers
      { customer-id: customer-id }
      (merge customer { lifetime-value: (+ (get lifetime-value customer) value) })
    )

    (ok (+ (get lifetime-value customer) value))
  )
)

;; Update journey analytics
(define-public (update-analytics
  (customer-id uint)
  (conversion-rate uint)
  (session-duration uint)
  (active-channel (string-ascii 20))
  (churn-risk uint)
  (next-action (string-ascii 100))
)
  (let
    (
      (analytics (unwrap! (map-get? journey-analytics { customer-id: customer-id }) err-not-found))
    )
    (asserts! (<= conversion-rate u100) err-invalid-input)
    (asserts! (<= churn-risk u100) err-invalid-input)

    (map-set journey-analytics
      { customer-id: customer-id }
      (merge analytics {
        conversion-rate: conversion-rate,
        average-session-duration: session-duration,
        most-active-channel: active-channel,
        churn-risk: churn-risk,
        next-best-action: next-action
      })
    )

    (ok true)
  )
)

;; Read-only Functions

;; Get customer information
(define-read-only (get-customer (customer-id uint))
  (map-get? customers { customer-id: customer-id })
)

;; Get customer by wallet
(define-read-only (get-customer-by-wallet (wallet principal))
  (match (map-get? customer-by-wallet { wallet: wallet })
    customer-ref (map-get? customers { customer-id: (get customer-id customer-ref) })
    none
  )
)

;; Get interaction details
(define-read-only (get-interaction (interaction-id uint))
  (map-get? interactions { interaction-id: interaction-id })
)

;; Get customer preferences
(define-read-only (get-preferences (customer-id uint))
  (map-get? customer-preferences { customer-id: customer-id })
)

;; Get journey analytics
(define-read-only (get-analytics (customer-id uint))
  (map-get? journey-analytics { customer-id: customer-id })
)

;; Get customer loyalty score
(define-read-only (get-loyalty-score (customer-id uint))
  (match (map-get? customers { customer-id: customer-id })
    customer (some (get loyalty-score customer))
    none
  )
)

;; Get lifetime value
(define-read-only (get-lifetime-value (customer-id uint))
  (match (map-get? customers { customer-id: customer-id })
    customer (some (get lifetime-value customer))
    none
  )
)
