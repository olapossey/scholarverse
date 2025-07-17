;; ScholarVerse - Freelance DAO Marketplace with NFT Bidding and Lottery
;; A robust SIP-009 NFT marketplace with royalties, DAO governance, auction bidding, and an integrated lottery module for fair user incentives.

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
(define-constant err-not-seller (err u102))
(define-constant err-token-listed (err u103))
(define-constant err-token-not-listed (err u104))
(define-constant err-transfer-failed (err u105))
(define-constant err-invalid-price (err u106))
(define-constant err-invalid-contract (err u107))
(define-constant err-invalid-token (err u108))
(define-constant err-bid-too-low (err u109))

(define-trait nft-trait 
  ((get-last-token-id () (response uint uint))
   (get-token-uri (uint) (response (optional (string-ascii 256)) uint))
   (get-owner (uint) (response (optional principal) uint))
   (transfer (uint principal principal) (response bool uint))))

(define-map listings
  { token-id: uint }
  { seller: principal, price: uint, nft: principal })

(define-map royalties
  { nft: principal }
  { rate: uint })

(define-map bids
  { token-id: uint }
  { bidder: principal, amount: uint })

(define-private (is-owner)
  (is-eq tx-sender contract-owner))

(define-private (get-listing (token-id uint))
  (map-get? listings { token-id: token-id }))

(define-private (check-is-owner (token-id uint) (nft-contract <nft-trait>))
  (let ((owner-opt (try! (contract-call? nft-contract get-owner token-id))))
    (match owner-opt 
      owner (if (is-eq tx-sender owner) 
                (ok true)
                err-not-token-owner)
      err-not-token-owner)))

(define-private (get-royalty-rate (nft principal))
  (get rate 
    (default-to 
      { rate: u0 } 
      (map-get? royalties { nft: nft }))))

(define-private (handle-royalty (price uint) (royalty uint))
  (if (> royalty u0)
    (stx-transfer? royalty tx-sender contract-owner)
    (ok true)))

;; === Marketplace Functions ===

(define-public (list-token (nft-contract <nft-trait>) (token-id uint) (price uint))
  (begin
    (asserts! (> price u0) err-invalid-price)
    (let ((owner-response (contract-call? nft-contract get-owner token-id)))
      (match owner-response owner
        (match owner actual-owner
          (let ((listing-opt (map-get? listings { token-id: token-id })))
            (if (is-none listing-opt)
              (begin
                (map-set listings { token-id: token-id } { seller: tx-sender, price: price, nft: (contract-of nft-contract) })
                (ok true))
              err-token-listed))
          err-not-token-owner)
        err err-not-token-owner))))

(define-public (cancel-listing (nft-contract principal) (token-id uint))
  (let ((listing (map-get? listings { token-id: token-id })))
    (match listing l
      (if (is-eq tx-sender (get seller l))
        (begin
          (map-delete listings { token-id: token-id })
          (ok true))
        err-not-seller)
      err-token-not-listed)))

(define-public (buy-nft-token (nft-contract <nft-trait>) (token-id uint))
  (let ((listing (map-get? listings { token-id: token-id })))
    (match listing l
      (let (
        (price (get price l))
        (seller (get seller l))
        (royalty (get-royalty-rate (contract-of nft-contract)))
        (royalty-amount (if (> royalty u0) (/ (* price royalty) u100) u0))
        (seller-amount (- price royalty-amount))
      )
        (begin
          (asserts! (is-eq (get nft l) (contract-of nft-contract)) err-invalid-contract)
          (asserts! (>= (stx-get-balance tx-sender) price) err-invalid-price)
          (try! (stx-transfer? price tx-sender seller))
          (try!
            (if (> royalty-amount u0)
              (stx-transfer? royalty-amount seller contract-owner)
              (ok true)))
          (try! (stx-transfer? seller-amount seller seller))
          (map-delete listings { token-id: token-id })
          (ok true)))
      err-token-not-listed)))

(define-public (set-royalty (nft principal) (rate uint))
  (begin
    (asserts! (is-owner) err-owner-only)
    (map-set royalties { nft: nft } { rate: rate })
    (ok true)))

(define-public (place-bid (token-id uint) (amount uint))
  (let ((listing (map-get? listings { token-id: token-id })))
    (match listing l
      (let ((current-bid-tuple (default-to { amount: u0, bidder: tx-sender } (map-get? bids { token-id: token-id })))
            (price (get price l)))
        (let ((current-bid (get amount current-bid-tuple)))
          (if (> amount current-bid)
            (if (>= amount price)
              (begin
                (map-set bids { token-id: token-id } { bidder: tx-sender, amount: amount })
                (ok true))
              err-invalid-price)
            err-bid-too-low)))
      err-token-not-listed)))

(define-public (accept-bid (nft-contract <nft-trait>) (token-id uint))
  (let ((listing (map-get? listings { token-id: token-id }))
        (bid (map-get? bids { token-id: token-id })))
    (match listing l
      (match bid b
        (let (
          (seller (get seller l))
          (bidder (get bidder b))
          (amount (get amount b))
          (royalty (get-royalty-rate (contract-of nft-contract)))
          (royalty-amount (if (> royalty u0) (/ (* amount royalty) u100) u0))
          (seller-amount (- amount royalty-amount))
        )
          (begin
            (asserts! (is-eq tx-sender seller) err-not-seller)
            (try! (contract-call? nft-contract transfer token-id seller bidder))
            (try!
              (if (> royalty-amount u0)
                (stx-transfer? royalty-amount bidder contract-owner)
                (ok true)))
            (try! (stx-transfer? seller-amount bidder seller))
            (map-delete listings { token-id: token-id })
            (map-delete bids { token-id: token-id })
            (ok true)))
        err-bid-too-low)
      err-token-not-listed)))

;; === Lottery Module ===
(define-constant entry-fee u10000000) ;; 10 STX (in microstacks)
(define-constant max-entries u100)
(define-map entries {round: uint} (list 100 principal))
(define-data-var lottery-open bool false)
(define-data-var admin principal tx-sender)
(define-data-var round uint u1)
(define-map winners uint principal)
(define-map rewards principal uint)

(define-private (only-admin)
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err u100))
    (ok true)))

(define-public (start-lottery)
  (begin
    (try! (only-admin))
    (map-set entries {round: (var-get round)} (list))
    (var-set lottery-open true)
    (ok true)))

(define-public (enter-lottery)
  (let ((current-entries (default-to (list) (map-get? entries {round: (var-get round)}))))
    (begin
      (asserts! (var-get lottery-open) (err u101))
      (try! (stx-transfer? entry-fee tx-sender (as-contract tx-sender)))
      (asserts! (is-none (index-of current-entries tx-sender)) (err u103))
      (asserts! (< (len current-entries) max-entries) (err u104))
      (let ((new-entries (unwrap! (as-max-len? (append current-entries tx-sender) u100) (err u105))))
        (map-set entries {round: (var-get round)} new-entries)
        (ok true)))))

(define-public (end-lottery)
  (begin
    (try! (only-admin))
    (asserts! (var-get lottery-open) (err u104))
    (let ((current-entries (unwrap! (map-get? entries {round: (var-get round)}) (err u105)))
          (count (len current-entries)))
      (asserts! (> count u0) (err u106))
      (let ((seed burn-block-height)
            (random (mod seed count))
            (winner (unwrap! (element-at current-entries random) (err u107)))
            (total-prize (* entry-fee count)))
        (begin
          (map-set winners (var-get round) winner)
          (map-set rewards winner total-prize)
          (var-set lottery-open false)
          (var-set round (+ (var-get round) u1))
          (map-insert entries {round: (var-get round)} (list))
          (ok true))))))

(define-public (claim-reward)
  (let ((amount (default-to u0 (map-get? rewards tx-sender))))
    (begin
      (asserts! (> amount u0) (err u108))
      (try! (stx-transfer? amount (as-contract tx-sender) tx-sender))
      (map-delete rewards tx-sender)
      (ok true))))

(define-read-only (get-entries)
  (ok (map-get? entries {round: (var-get round)})))

(define-read-only (get-winner (r uint))
  (ok (map-get? winners r)))

(define-read-only (get-reward-balance (user principal))
  (ok (map-get? rewards user)))
