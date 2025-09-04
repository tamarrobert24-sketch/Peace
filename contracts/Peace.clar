;; mini-rfp.clar
;; Brief & unique winning contract for Google Clarity Web3 RFPs

(clarity-version 2)

(define-data-var rfp-id uint u0)
(define-map rfps ((id uint)) ((owner principal) (winner (optional principal))))
(define-map commits ((id uint) (vendor principal)) ((h (buff 32))))
(define-map reveals ((id uint) (vendor principal)) ((proposal (string-utf8 30))))

;; Create RFP
(define-public (create-rfp)
  (let ((id (+ (var-get rfp-id) u1)))
    (map-set rfps { id: id } { owner: tx-sender, winner: none })
    (var-set rfp-id id)
    (ok id)))

;; Vendor commits (sha256 of proposal||salt)
(define-public (commit (id uint) (h (buff 32)))
  (map-set commits { id: id, vendor: tx-sender } { h: h })
  (ok true))

;; Vendor reveals proposal
(define-public (reveal (id uint) (proposal (string-utf8 30)) (salt (buff 32)))
  (let ((c (map-get? commits { id: id, vendor: tx-sender })))
    (match c
      some (if (is-eq (get h some) (sha256 (concat (utf8-to-bytes proposal) salt)))
             (begin (map-set reveals { id: id, vendor: tx-sender } { proposal: proposal }) (ok true))
             (err u100))
      none (err u101))))

;; Finalize winner
(define-public (finalize (id uint) (winner principal))
  (begin
    (asserts! (is-eq (get owner (unwrap! (map-get? rfps { id: id }) (err u102))) tx-sender) (err u103))
    (map-set rfps { id: id } { owner: tx-sender, winner: (some winner) })
    (ok winner)))
