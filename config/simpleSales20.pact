(module simplesales20 GOVERNANCE

  (bless "AkuzM_RcFLl-7l7bpAmfofZMdeKgC_6ZDzs29dIQTu8")

  (use fungible-util)
  (use util.guards)

  (defcap GOVERNANCE ()
  (with-read admin20 "admin"
    { "aguard" := aguard }
    (enforce-guard aguard)
  ))

  (defcap INTERNAL ()
    "mark some functions as internal only"
    true
  )

  (defconst ZTRUST_BANK:string 'ztrust-bank20)
  (defconst percFee:decimal 0.005)
  (defconst CF:string "CF")
  (defconst SL:string "SL")
  (defconst VP:string "VP")
  (defconst SF:string "SF")
  (defconst TO:string "TO")
  (defconst TRN:string "{\"trn\":\"")
  (defconst TN:string "1122334455")
  (defconst zeroAmount:decimal 0.0)
  (defconst DELIVERED:string "Delivered")
  (defconst MILLION:decimal 1000000.0)

  (defun ztrust-bank-guard () (create-module-guard "keyset"))

  (defun init (aguard:guard)
    "Create Bank and set admin..."
    ; (coin.create-account ZTRUST_BANK (ztrust-bank-guard))
    ; (insert admin20 "admin" {
    ;   "admin": (at "sender" (chain-data)),
    ;   "aguard": aguard
    ;   } )
    (free.pactAgent18.register-module "free.simplesales20" 600)
    (free.pactAgent18.create-script "free.simplesales20" "usps" 3600 1 5 90 "getWebData"
     "\"url\":\"https://tools.usps.com/go/TrackConfirmAction?tRef=fullpage&tLc=2&text28777=&tLabels=${trn}\",\"selector\":\"span.text_explanation\"")
  )

  (defun crank ()
    (with-capability (INTERNAL)
     (update-agentResults)
     (checkTimeout)
     (free.pactAgent18.set-module-next "free.simplesales20")
    )
  )

  (defschema admin
    admin:string
    aguard:guard
  )

  (deftable admin20:{admin})

  (defschema ledger
    subContractId:string
    entryType:string
    received:bool
    account:string
    amount:decimal
    eventAt:time
  )

  (deftable ledger20:{ledger})

  (defschema subContract
    subContractId:string
    subContractNumber:string
    seller:string
    sguard:guard
    buyer:string
    value:decimal
    timeout:integer
    funded:decimal
    shipper:string
    trackingNumber:string
    delivered:bool
    refunded:bool
    createdAt:time
    timesOut:time)

  (deftable subContracts20:{subContract})

  (defcap CREDIT (receiver:string) true)
  (defcap DEBIT (sender:string) true)

  (defun fund:string (subContractId:string amount:decimal)
    "Fund a subContract"
    (with-read subContracts20 subContractId
      { "value":= value,
        "funded":= funded,
        "timeout":= timeout,
        "timesOut":= timesOut,
        "createdAt":= createdAt,
        "buyer":= account }
      (enforce (= funded 0.0) "Must not yet be funded")
      (enforce (= value amount) "Must fund with the exact value")
      (enforce (< (get-time) timesOut) "Not timed out yet")
      (coin.transfer account ZTRUST_BANK amount)
      (update subContracts20 subContractId
        { "funded": amount })
      (let
        ((tx-id (hash {"subContractId": subContractId, "entryType": CF, "account": account, "salt": (at "block-time" (chain-data))})))
        (insert ledger20 tx-id
            { "subContractId": subContractId,
              "entryType": CF,
              "received": false,
              "account": account,
              "amount": amount,
              "eventAt": (get-time)}
        )
      )
    )
  )

  (defun updateShipping:string (subContractId:string shipper:string trackingNumber:string)
    "Update shipping info"
    (with-read subContracts20 subContractId
      { "sguard" := sguard,
        "value":= value,
        "funded":= funded,
        "timeout":= timeout,
        "timesOut":= timesOut,
        "createdAt":= createdAt,
        "seller" := account }
      (enforce-guard sguard)
      (enforce (= value funded) "Must be fully funded")
      (enforce (< (get-time) timesOut) "Not timed out yet")
      (update subContracts20 subContractId
      { "shipper": shipper,
        "trackingNumber": trackingNumber}
      )
      ; Insert task to check delivery periodically
      (let ((input (+ (+ TRN trackingNumber) "\"}")))
      (free.pactAgent18.create-task "free.simplesales20" "usps" subContractId input (* timeout 24) DELIVERED)) 
      ; Insert ledger entry
      (let
        ((tx-id (hash {"subContractId": subContractId, "entryType": SL, "account": account, "salt": (at "block-time" (chain-data))})))
        (insert ledger20 tx-id
            { "subContractId": subContractId,
              "entryType": SL,
              "received": false,
              "account": account,
              "amount": zeroAmount,
              "eventAt": (get-time)}
        )
      )
    )
  )

  (defun close-contract:string (sf:string subContract:object{subContract})
      (require-capability (INTERNAL))
      (bind subContract { 
        "subContractId" := subContractId,
        "buyer" := buyer,
        "value" := value,
        "funded" := funded,
        "delivered" := delivered,
        "refunded" := refunded }
        (if (= value funded) 
        (if (= false delivered)
        (if (= false refunded)
          (refund subContractId buyer value)
        "") "") "")
      )
  )

  (defun refund:string (subContractId:string buyer:string value:decimal)
    (require-capability (INTERNAL))
    (install-capability (coin.TRANSFER ZTRUST_BANK buyer value))
    (coin.transfer ZTRUST_BANK buyer value)
    (update subContracts20 subContractId
        { "funded": 0.0,
          "refunded": true})
    (let
        ((tx-id (hash {"subContractId": subContractId, "entryType": TO, "account": buyer, "salt": (at "block-time" (chain-data))})))
        (insert ledger20 tx-id
            { "subContractId": subContractId,
              "entryType": TO,
              "received": false,
              "account": buyer,
              "amount": value,
              "eventAt": (get-time)}
        )
    )
  )

  (defun checkTimeout ()
    "Check for timed out contracts"
    (require-capability (INTERNAL))
    (let ((cont (select subContracts20 
        (and?
        (where 'timesOut (> (get-time)))
        (where 'delivered (= false)) 
        ))))
        (fold (close-contract) "" cont) 
    )    
  )

  (defun listTimedOut ()
    "List timed out contracts"
     (select subContracts20 (where 'timesOut (> (get-time))))
  )

  (defun setDelivered (subContractId)
    "Set delivered"
    (with-read admin20 "admin"
      { "admin" := admin,
        "aguard" := aguard }
    ; (require-capability (INTERNAL))
    (with-read subContracts20 subContractId
          { "seller" := seller,
            "value" := value,
            "funded" := funded,
            "shipper":= shipper,
            "delivered" := delivered,
            "trackingNumber":= trackingNumber }
          (if (= value funded) 
          (if (= false delivered) 
          (if (!= "" shipper) 
          (if (!= "" trackingNumber) 
          (let
            ( (sellerGets:decimal (* value (- 1 percFee))))   
            (install-capability (coin.TRANSFER ZTRUST_BANK seller sellerGets))
            (coin.transfer ZTRUST_BANK seller sellerGets)
            (update subContracts20 subContractId
              { "delivered": true })
            (let
                ((tx-id (hash {"subContractId": subContractId, "entryType": VP, "account": seller, "salt": (get-time)})))
                (insert ledger20 tx-id
                    { "subContractId": subContractId,
                      "entryType": VP,
                      "received": true,
                      "account": seller,
                      "amount": sellerGets,
                      "eventAt": (at "block-time" (chain-data))}
                )
            )
            (install-capability (coin.TRANSFER ZTRUST_BANK admin (- value sellerGets))) 
            (coin.transfer ZTRUST_BANK admin (- value sellerGets))
              (let
                ((tx-id (hash {"subContractId": subContractId, "entryType": SF, "account": admin, "salt": (get-time)})))
                (insert ledger20 tx-id
                    { "subContractId": subContractId,
                      "entryType": SF,
                      "received": true,
                      "account": admin,
                      "amount": (- value sellerGets),
                      "eventAt": (get-time)}
                )
              )
        ) "" ) "" ) "" ) "" )
    )
    )
  )
 
  (defun get-time:time ()
    "Get block time"
    (at "block-time" (chain-data))
  )

  (defun create-subContract (subContractId subContractNumber buyer value timeout)
    "Create a subContract"
    (enforce (>= value 0.0) "Value must be >= 0.")
    (enforce (> timeout 0) "Timeout must be > 0")
    (enforce (!= "" subContractId) "subContractId cannot be empty")
    (enforce (!= "" subContractNumber) "subContractNumber cannot be empty")
    (enforce (!= "" buyer) "buyer cannot be empty")
    (insert subContracts20 subContractId
            { "subContractId": subContractId,
              "subContractNumber": subContractNumber,
              "seller": (at "sender" (chain-data)),
              "sguard": (read-keyset "keyset"),
              "buyer": buyer,
              "value": value,
              "timeout": timeout,
              "funded": 0.0,
              "shipper": "",
              "trackingNumber": "",
              "delivered": false,
              "refunded": false,
              "createdAt": (get-time),
              "timesOut": (add-time (get-time) (days timeout))}
    )
  )

  (defun get-ledgerForAccount (account:string)
    "Get ledger entries for an account"
    (select ledger20 (where 'account (= account)))
  )

  (defun get-contractsForAccount (account:string)
    "Get subContracts for an account"
    (select subContracts20 (or? (where 'seller (= account)) (where 'buyer (= account))))
  )

  (defun update-agentResults ()
    "Get agent results and update contracts accordingly"
    (require-capability (INTERNAL))
    (let
      (
        (cont (select subContracts20
          (and? 
            (where 'delivered (= false)) 
            (where 'refunded (= false))
          )
        ))
      )
      (fold (get-agentResult) "" cont)    
    )
  )
  
  (defun get-agentResult:string (sf:string subContract:object{subContract})
    "Get agent result and process"
    (require-capability (INTERNAL))
    (bind subContract 
      {"subContractId" := subContractId}
        (let
          ((result (free.pactAgent18.get-result subContractId)))
          (if (= result DELIVERED) (setDelivered subContractId) "")     
        )
    )
  )

  (defun get-allContracts ()
    "Get subContracts"
    (select subContracts20
          (and? 
            (where 'delivered (= false)) 
            (where 'refunded (= false))
          )
    )
  )

  (defun microseconds ()
    "Microseconds"
      (/ (str-to-int (format-time "%v" (get-time))) MILLION)
  )

  (defun test-string ()
    "test string"
    (+ (+ TRN TN) "}")
  )

  (defun read-admin ()
    "Read admin guard"
    (with-read admin20 "admin"
      { "admin":= admin,
      "aguard":= aguard }
    (format "Admin guard is {}, admin {}" [aguard admin]))
  )

  (defun test-admin ()
    "Test admin guard"
  (with-read admin20 "admin"
    { "aguard" := aguard }
    (enforce-guard aguard)
      (format "Admin guard success! {}" [aguard] )
    )
  )

  (defun read-subContract (subContractId)
    "Read a subContract"
    (read subContracts20 subContractId) 
  )

  (defun show-msg ()
    (free.pactAgent18.show-msg)
  )
)
; create-table
(create-table subContracts20)
(create-table admin20)
(create-table ledger20)




