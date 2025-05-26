;; Channel Integration Contract
;; Connects retail touchpoints and manages channel configurations

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u200))
(define-constant err-not-found (err u201))
(define-constant err-already-exists (err u202))
(define-constant err-invalid-input (err u203))
(define-constant err-unauthorized (err u204))

;; Data Variables
(define-data-var next-channel-id uint u1)

;; Data Maps
(define-map channels
  { channel-id: uint }
  {
    retailer-id: uint,
    channel-type: (string-ascii 20),
    name: (string-ascii 100),
    active: bool,
    created-block: uint,
    last-sync: uint,
    total-transactions: uint,
    configuration: (string-ascii 500)
  }
)

(define-map retailer-channels
  { retailer-id: uint, channel-type: (string-ascii 20) }
  { channel-id: uint }
)

(define-map channel-sync-log
  { channel-id: uint, sync-id: uint }
  {
    sync-timestamp: uint,
    data-hash: (string-ascii 64),
    sync-status: (string-ascii 20),
    error-message: (optional (string-ascii 200))
  }
)

(define-map channel-metrics
  { channel-id: uint }
  {
    daily-transactions: uint,
    weekly-revenue: uint,
    customer-satisfaction: uint,
    uptime-percentage: uint,
    last-updated: uint
  }
)

;; Public Functions

;; Add a new channel
(define-public (add-channel
  (retailer-id uint)
  (channel-type (string-ascii 20))
  (name (string-ascii 100))
  (configuration (string-ascii 500))
)
  (let
    (
      (channel-id (var-get next-channel-id))
    )
    (asserts! (> (len name) u0) err-invalid-input)
    (asserts! (> (len channel-type) u0) err-invalid-input)
    (asserts! (is-none (map-get? retailer-channels { retailer-id: retailer-id, channel-type: channel-type })) err-already-exists)

    ;; Create channel record
    (map-set channels
      { channel-id: channel-id }
      {
        retailer-id: retailer-id,
        channel-type: channel-type,
        name: name,
        active: true,
        created-block: block-height,
        last-sync: block-height,
        total-transactions: u0,
        configuration: configuration
      }
    )

    ;; Map retailer to channel
    (map-set retailer-channels
      { retailer-id: retailer-id, channel-type: channel-type }
      { channel-id: channel-id }
    )

    ;; Initialize metrics
    (map-set channel-metrics
      { channel-id: channel-id }
      {
        daily-transactions: u0,
        weekly-revenue: u0,
        customer-satisfaction: u80,
        uptime-percentage: u100,
        last-updated: block-height
      }
    )

    ;; Increment next ID
    (var-set next-channel-id (+ channel-id u1))

    (ok channel-id)
  )
)

;; Configure channel settings
(define-public (configure-channel (channel-id uint) (new-configuration (string-ascii 500)))
  (let
    (
      (channel (unwrap! (map-get? channels { channel-id: channel-id }) err-not-found))
    )
    (asserts! (> (len new-configuration) u0) err-invalid-input)

    (map-set channels
      { channel-id: channel-id }
      (merge channel { configuration: new-configuration })
    )

    (ok true)
  )
)

;; Sync channel data
(define-public (sync-channel (channel-id uint) (data-hash (string-ascii 64)))
  (let
    (
      (channel (unwrap! (map-get? channels { channel-id: channel-id }) err-not-found))
      (sync-id (get total-transactions channel))
    )
    (asserts! (get active channel) err-unauthorized)

    ;; Update channel sync timestamp
    (map-set channels
      { channel-id: channel-id }
      (merge channel { last-sync: block-height })
    )

    ;; Log sync operation
    (map-set channel-sync-log
      { channel-id: channel-id, sync-id: sync-id }
      {
        sync-timestamp: block-height,
        data-hash: data-hash,
        sync-status: "completed",
        error-message: none
      }
    )

    (ok sync-id)
  )
)

;; Record transaction
(define-public (record-transaction (channel-id uint) (transaction-amount uint))
  (let
    (
      (channel (unwrap! (map-get? channels { channel-id: channel-id }) err-not-found))
      (metrics (unwrap! (map-get? channel-metrics { channel-id: channel-id }) err-not-found))
    )
    (asserts! (get active channel) err-unauthorized)

    ;; Update channel transaction count
    (map-set channels
      { channel-id: channel-id }
      (merge channel { total-transactions: (+ (get total-transactions channel) u1) })
    )

    ;; Update metrics
    (map-set channel-metrics
      { channel-id: channel-id }
      (merge metrics {
        daily-transactions: (+ (get daily-transactions metrics) u1),
        weekly-revenue: (+ (get weekly-revenue metrics) transaction-amount),
        last-updated: block-height
      })
    )

    (ok (+ (get total-transactions channel) u1))
  )
)

;; Toggle channel active status
(define-public (toggle-channel-status (channel-id uint))
  (let
    (
      (channel (unwrap! (map-get? channels { channel-id: channel-id }) err-not-found))
    )
    (map-set channels
      { channel-id: channel-id }
      (merge channel { active: (not (get active channel)) })
    )

    (ok (not (get active channel)))
  )
)

;; Update channel metrics
(define-public (update-metrics
  (channel-id uint)
  (satisfaction uint)
  (uptime uint)
)
  (let
    (
      (metrics (unwrap! (map-get? channel-metrics { channel-id: channel-id }) err-not-found))
    )
    (asserts! (<= satisfaction u100) err-invalid-input)
    (asserts! (<= uptime u100) err-invalid-input)

    (map-set channel-metrics
      { channel-id: channel-id }
      (merge metrics {
        customer-satisfaction: satisfaction,
        uptime-percentage: uptime,
        last-updated: block-height
      })
    )

    (ok true)
  )
)

;; Read-only Functions

;; Get channel information
(define-read-only (get-channel (channel-id uint))
  (map-get? channels { channel-id: channel-id })
)

;; Get channel by retailer and type
(define-read-only (get-retailer-channel (retailer-id uint) (channel-type (string-ascii 20)))
  (match (map-get? retailer-channels { retailer-id: retailer-id, channel-type: channel-type })
    channel-ref (map-get? channels { channel-id: (get channel-id channel-ref) })
    none
  )
)

;; Get channel metrics
(define-read-only (get-channel-metrics (channel-id uint))
  (map-get? channel-metrics { channel-id: channel-id })
)

;; Get sync log entry
(define-read-only (get-sync-log (channel-id uint) (sync-id uint))
  (map-get? channel-sync-log { channel-id: channel-id, sync-id: sync-id })
)

;; Check if channel is active
(define-read-only (is-channel-active (channel-id uint))
  (match (map-get? channels { channel-id: channel-id })
    channel (get active channel)
    false
  )
)

;; Get channel configuration
(define-read-only (get-channel-config (channel-id uint))
  (match (map-get? channels { channel-id: channel-id })
    channel (some (get configuration channel))
    none
  )
)
