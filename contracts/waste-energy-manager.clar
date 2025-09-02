;; Waste-to-Energy Tracking Smart Contract
;; Municipal waste management system with collection optimization, processing monitoring, and energy generation measurement

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_INVALID_INPUT (err u102))
(define-constant ERR_FACILITY_INACTIVE (err u103))

;; Data Variables
(define-data-var facility-counter uint u0)
(define-data-var collection-counter uint u0)
(define-data-var processing-counter uint u0)

;; Data Maps
(define-map facilities
  { facility-id: uint }
  {
    name: (string-ascii 50),
    location: (string-ascii 100),
    capacity: uint, ;; tons per day
    energy-output-rate: uint, ;; kwh per ton
    active: bool,
    manager: principal,
    created-at: uint
  }
)

(define-map waste-collections
  { collection-id: uint }
  {
    facility-id: uint,
    waste-type: (string-ascii 30),
    amount: uint, ;; in tons
    collection-date: uint,
    route-optimization-score: uint, ;; 1-100
    collector: principal
  }
)

(define-map processing-batches
  { batch-id: uint }
  {
    facility-id: uint,
    waste-amount: uint, ;; in tons
    energy-generated: uint, ;; in kwh
    efficiency-rating: uint, ;; 1-100
    processing-date: uint,
    operator: principal
  }
)

(define-map facility-managers
  { facility-id: uint, manager: principal }
  { authorized: bool }
)

;; Read-only functions
(define-read-only (get-facility (facility-id uint))
  (map-get? facilities { facility-id: facility-id })
)

(define-read-only (get-waste-collection (collection-id uint))
  (map-get? waste-collections { collection-id: collection-id })
)

(define-read-only (get-processing-batch (batch-id uint))
  (map-get? processing-batches { batch-id: batch-id })
)

(define-read-only (is-facility-manager (facility-id uint) (manager principal))
  (default-to false
    (get authorized (map-get? facility-managers { facility-id: facility-id, manager: manager }))
  )
)

(define-read-only (get-facility-total-energy (facility-id uint))
  (let ((facility (unwrap! (get-facility facility-id) (err u0))))
    (if (get active facility)
      (ok (* (get capacity facility) (get energy-output-rate facility)))
      (err u0)
    )
  )
)

(define-read-only (calculate-energy-efficiency (batch-id uint))
  (match (get-processing-batch batch-id)
    batch-data
      (let (
        (expected-energy (* (get waste-amount batch-data) u50)) ;; baseline 50 kwh per ton
        (actual-energy (get energy-generated batch-data))
      )
        (if (> expected-energy u0)
          (ok (/ (* actual-energy u100) expected-energy))
          (err u0)
        )
      )
    (err u0)
  )
)

(define-read-only (get-counters)
  {
    facilities: (var-get facility-counter),
    collections: (var-get collection-counter),
    processing-batches: (var-get processing-counter)
  }
)

;; Public functions
(define-public (register-facility (name (string-ascii 50)) (location (string-ascii 100)) (capacity uint) (energy-output-rate uint) (manager principal))
  (let ((facility-id (+ (var-get facility-counter) u1)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (and (> capacity u0) (> energy-output-rate u0)) ERR_INVALID_INPUT)
    
    (map-set facilities
      { facility-id: facility-id }
      {
        name: name,
        location: location,
        capacity: capacity,
        energy-output-rate: energy-output-rate,
        active: true,
        manager: manager,
        created-at: stacks-block-height
      }
    )
    
    (map-set facility-managers
      { facility-id: facility-id, manager: manager }
      { authorized: true }
    )
    
    (var-set facility-counter facility-id)
    (ok facility-id)
  )
)

(define-public (record-waste-collection (facility-id uint) (waste-type (string-ascii 30)) (amount uint) (route-optimization-score uint))
  (let (
    (collection-id (+ (var-get collection-counter) u1))
    (facility (unwrap! (get-facility facility-id) ERR_NOT_FOUND))
  )
    (asserts! (get active facility) ERR_FACILITY_INACTIVE)
    (asserts! (or (is-facility-manager facility-id tx-sender) (is-eq tx-sender CONTRACT_OWNER)) ERR_UNAUTHORIZED)
    (asserts! (and (> amount u0) (<= route-optimization-score u100)) ERR_INVALID_INPUT)
    
    (map-set waste-collections
      { collection-id: collection-id }
      {
        facility-id: facility-id,
        waste-type: waste-type,
        amount: amount,
        collection-date: stacks-block-height,
        route-optimization-score: route-optimization-score,
        collector: tx-sender
      }
    )
    
    (var-set collection-counter collection-id)
    (ok collection-id)
  )
)

(define-public (record-processing-batch (facility-id uint) (waste-amount uint) (energy-generated uint) (efficiency-rating uint))
  (let (
    (batch-id (+ (var-get processing-counter) u1))
    (facility (unwrap! (get-facility facility-id) ERR_NOT_FOUND))
  )
    (asserts! (get active facility) ERR_FACILITY_INACTIVE)
    (asserts! (or (is-facility-manager facility-id tx-sender) (is-eq tx-sender CONTRACT_OWNER)) ERR_UNAUTHORIZED)
    (asserts! (and (> waste-amount u0) (> energy-generated u0) (<= efficiency-rating u100)) ERR_INVALID_INPUT)
    
    (map-set processing-batches
      { batch-id: batch-id }
      {
        facility-id: facility-id,
        waste-amount: waste-amount,
        energy-generated: energy-generated,
        efficiency-rating: efficiency-rating,
        processing-date: stacks-block-height,
        operator: tx-sender
      }
    )
    
    (var-set processing-counter batch-id)
    (ok batch-id)
  )
)

(define-public (add-facility-manager (facility-id uint) (manager principal))
  (let ((facility (unwrap! (get-facility facility-id) ERR_NOT_FOUND)))
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER) (is-eq tx-sender (get manager facility))) ERR_UNAUTHORIZED)
    
    (map-set facility-managers
      { facility-id: facility-id, manager: manager }
      { authorized: true }
    )
    
    (ok true)
  )
)

(define-public (remove-facility-manager (facility-id uint) (manager principal))
  (let ((facility (unwrap! (get-facility facility-id) ERR_NOT_FOUND)))
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER) (is-eq tx-sender (get manager facility))) ERR_UNAUTHORIZED)
    (asserts! (not (is-eq manager (get manager facility))) ERR_UNAUTHORIZED) ;; Cannot remove primary manager
    
    (map-delete facility-managers { facility-id: facility-id, manager: manager })
    (ok true)
  )
)

(define-public (update-facility-status (facility-id uint) (active bool))
  (let ((facility (unwrap! (get-facility facility-id) ERR_NOT_FOUND)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    
    (map-set facilities
      { facility-id: facility-id }
      (merge facility { active: active })
    )
    
    (ok true)
  )
)

(define-public (transfer-facility-ownership (facility-id uint) (new-manager principal))
  (let ((facility (unwrap! (get-facility facility-id) ERR_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get manager facility)) ERR_UNAUTHORIZED)
    
    ;; Update facility with new manager
    (map-set facilities
      { facility-id: facility-id }
      (merge facility { manager: new-manager })
    )
    
    ;; Add new manager to authorized list
    (map-set facility-managers
      { facility-id: facility-id, manager: new-manager }
      { authorized: true }
    )
    
    (ok true)
  )
)

