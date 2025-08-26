;; DigitalArt Marketplace Contract
;; Creator-first NFT platform with reduced fees and enhanced artist protection features

;; Define the NFT
(define-non-fungible-token digital-art uint)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-nft-not-found (err u102))
(define-constant err-invalid-price (err u103))
(define-constant err-insufficient-payment (err u104))
(define-constant err-already-listed (err u105))

;; Reduced marketplace fee (2% instead of typical 5-10%)
(define-constant marketplace-fee-percentage u200) ;; 2% in basis points (200/10000)

;; Data variables
(define-data-var next-nft-id uint u1)
(define-data-var total-marketplace-earnings uint u0)

;; Maps for NFT marketplace data
(define-map nft-listings 
  uint 
  {
    creator: principal,
    price: uint,
    is-active: bool,
    royalty-percentage: uint ;; Artist royalty protection (basis points)
  }
)

(define-map nft-metadata
  uint
  {
    title: (string-ascii 64),
    description: (string-ascii 256),
    image-uri: (string-ascii 256),
    creator: principal
  }
)

;; Function 1: Mint and List NFT
;; Enhanced artist protection with built-in royalties and reduced fees
(define-public (mint-and-list-nft 
  (title (string-ascii 64))
  (description (string-ascii 256)) 
  (image-uri (string-ascii 256))
  (price uint)
  (royalty-percentage uint))
  (let 
    (
      (nft-id (var-get next-nft-id))
    )
    (begin
      ;; Validate inputs
      (asserts! (> price u0) err-invalid-price)
      (asserts! (<= royalty-percentage u1000) err-invalid-price) ;; Max 10% royalty
      
      ;; Mint NFT to creator
      (try! (nft-mint? digital-art nft-id tx-sender))
      
      ;; Store metadata
      (map-set nft-metadata nft-id {
        title: title,
        description: description,
        image-uri: image-uri,
        creator: tx-sender
      })
      
      ;; List NFT for sale with artist protection
      (map-set nft-listings nft-id {
        creator: tx-sender,
        price: price,
        is-active: true,
        royalty-percentage: royalty-percentage
      })
      
      ;; Increment NFT ID counter
      (var-set next-nft-id (+ nft-id u1))
      
      ;; Return success with NFT ID
      (ok nft-id))))

;; Function 2: Purchase NFT with Creator Royalties
;; Implements reduced marketplace fees and automatic royalty distribution
(define-public (purchase-nft (nft-id uint))
  (let
    (
      (listing (unwrap! (map-get? nft-listings nft-id) err-nft-not-found))
      (nft-price (get price listing))
      (creator (get creator listing))
      (royalty-rate (get royalty-percentage listing))
      (current-owner (unwrap! (nft-get-owner? digital-art nft-id) err-nft-not-found))
      
      ;; Calculate fees and payments
      (marketplace-fee (/ (* nft-price marketplace-fee-percentage) u10000))
      (royalty-payment (/ (* nft-price royalty-rate) u10000))
      (seller-payment (- (- nft-price marketplace-fee) royalty-payment))
    )
    (begin
      ;; Validate purchase conditions
      (asserts! (get is-active listing) err-nft-not-found)
      (asserts! (>= (stx-get-balance tx-sender) nft-price) err-insufficient-payment)
      (asserts! (not (is-eq tx-sender current-owner)) err-not-authorized)
      
      ;; Transfer payment to seller (reduced fee means more money to seller)
      (try! (stx-transfer? seller-payment tx-sender current-owner))
      
      ;; Pay royalty to original creator (artist protection)
      (if (and (> royalty-payment u0) (not (is-eq creator current-owner)))
        (try! (stx-transfer? royalty-payment tx-sender creator))
        true)
      
      ;; Transfer marketplace fee to contract
      (try! (stx-transfer? marketplace-fee tx-sender (as-contract tx-sender)))
      
      ;; Update marketplace earnings
      (var-set total-marketplace-earnings 
        (+ (var-get total-marketplace-earnings) marketplace-fee))
      
      ;; Transfer NFT to buyer
      (try! (nft-transfer? digital-art nft-id current-owner tx-sender))
      
      ;; Remove from active listings
      (map-set nft-listings nft-id 
        (merge listing { is-active: false }))
      
      ;; Return success
      (ok true))))

;; Read-only functions for marketplace info
(define-read-only (get-nft-listing (nft-id uint))
  (map-get? nft-listings nft-id))

(define-read-only (get-nft-metadata (nft-id uint))
  (map-get? nft-metadata nft-id))

(define-read-only (get-nft-owner (nft-id uint))
  (nft-get-owner? digital-art nft-id))

(define-read-only (get-marketplace-stats)
  (ok {
    total-earnings: (var-get total-marketplace-earnings),
    next-nft-id: (var-get next-nft-id),
    marketplace-fee: marketplace-fee-percentage
  }))