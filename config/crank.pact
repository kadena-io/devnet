(module crankk01 GOVERNANCE

  @doc " Adapted from Kadena's coin.pact contract and finprint. "

  @model
    [ (defproperty conserves-mass (amount:decimal)
        (= (column-delta crankk01-token-table 'balance) 0.0))

      (defproperty valid-account-id (accountId:string)
        (and
          (>= (length accountId) 3)
          (<= (length accountId) 256)))
    ]

  (implements fungible-v2)

  ; --------------------------------------------------------------------------
  ; Schemas and Tables

  (defschema token-schema
    @doc " An account, holding a token balance. \
         \ \
         \ ROW KEY: accountId. "
    balance:decimal
    guard:guard
  )
  (deftable crankk01-token-table:{token-schema})

  ; --------------------------------------------------------------------------
  ; Capatilibites

  (defcap GOVERNANCE
    ()

    @doc " Give the admin full access to call and upgrade the module. "

    (enforce-keyset 'admin-crankk01)
  )

  (defcap ACCOUNT_GUARD
    ( accountId:string )
    @doc " Look up the guard for an account, required to debit from that account. "
    (enforce-guard (at 'guard (read crankk01-token-table accountId ['guard])))
  )

  (defcap DEBIT
    ( sender:string )

    @doc " Capability to perform debiting operations. "

    (enforce-guard (at 'guard (read crankk01-token-table sender ['guard])))
    (enforce (!= sender "") "Invalid sender.")
  )

  (defcap CREDIT
    ( receiver:string )

    @doc " Capability to perform crediting operations. "

    (enforce (!= receiver "") "Invalid receiver.")
  )

  (defcap TRANSFER:bool
    ( sender:string
      receiver:string
      amount:decimal )

    @doc " Capability to perform transfer between two accounts. "

    @managed amount TRANSFER-mgr

    (enforce (!= sender receiver) "Sender cannot be the receiver.")
    (enforce-unit amount)
    (enforce (> amount 0.0) "Transfer amount must be positive.")
    (compose-capability (DEBIT sender))
    (compose-capability (CREDIT receiver))
  )

  (defun TRANSFER-mgr:decimal
    ( managed:decimal
      requested:decimal )

    (let ((newbal (- managed requested)))
      (enforce (>= newbal 0.0)
        (format "TRANSFER exceeded for balance {}" [managed]))
      newbal
    )
  )

  ; --------------------------------------------------------------------------
  ; Constants

  (defconst ROOT_ACCOUNT_ID:string 'ROOT
    " ID for the account which initially owns all the tokens.  ")

  (defconst INITIAL_SUPPLY:decimal 500000.0
    " Initial supply of 10 million tokens. (500k x20 chains)")

  (defconst DECIMALS 12
    " Specifies the minimum denomination for token transactions. ")

  (defconst ACCOUNT_ID_CHARSET CHARSET_LATIN1
    " Allowed character set for account IDs. ")

  (defconst ACCOUNT_ID_PROHIBITED_CHARACTER "$")

  (defconst ACCOUNT_ID_MIN_LENGTH 3
    " Minimum character length for account IDs. ")

  (defconst ACCOUNT_ID_MAX_LENGTH 256
    " Maximum character length for account IDs. ")


  ; --------------------------------------------------------------------------
  ; Utilities

  (defun validate-account-id
    ( accountId:string )

    @doc " Enforce that an account ID meets charset and length requirements. "

    (enforce
      (is-charset ACCOUNT_ID_CHARSET accountId)
      (format
        "Account ID does not conform to the required charset: {}"
        [accountId]))

    (enforce
      (not (contains accountId ACCOUNT_ID_PROHIBITED_CHARACTER))
      (format "Account ID contained a prohibited character: {}" [accountId]))

    (let ((accountLength (length accountId)))

      (enforce
        (>= accountLength ACCOUNT_ID_MIN_LENGTH)
        (format
          "Account ID does not conform to the min length requirement: {}"
          [accountId]))

      (enforce
        (<= accountLength ACCOUNT_ID_MAX_LENGTH)
        (format
          "Account ID does not conform to the max length requirement: {}"
          [accountId]))
    )
  )

  ;; ; --------------------------------------------------------------------------
  ;; ; Fungible-v2 Implementation

  (defun transfer-create:string
    ( sender:string
      receiver:string
      receiver-guard:guard
      amount:decimal )

    @doc " Transfer to an account, creating it if it does not exist. "

    @model [ (property (conserves-mass amount))
             (property (> amount 0.0))
             (property (valid-account-id sender))
             (property (valid-account-id receiver))
             (property (!= sender receiver)) ]

    (with-capability (TRANSFER sender receiver amount)
      (debit sender amount)
      (credit receiver receiver-guard amount)
    )
  )

  (defun transfer:string
    ( sender:string
      receiver:string
      amount:decimal )

    @doc " Transfer to an account, failing if the account does not exist. "

    @model [ (property (conserves-mass amount))
             (property (> amount 0.0))
             (property (valid-account-id sender))
             (property (valid-account-id receiver))
             (property (!= sender receiver)) ]

    (with-read crankk01-token-table receiver
      { "guard" := guard }
      (transfer-create sender receiver guard amount)
    )
  )

  (defun debit
    ( accountId:string
      amount:decimal )

    @doc " Decrease an account balance. Internal use only. "

    @model [ (property (> amount 0.0))
             (property (valid-account-id accountId))
           ]

    (validate-account-id accountId)
    (enforce (> amount 0.0) "Debit amount must be positive.")
    (enforce-unit amount)
    (require-capability (DEBIT accountId))

    (with-read crankk01-token-table accountId
      { "balance" := balance }

      (enforce (<= amount balance) "Insufficient funds.")

      (update crankk01-token-table accountId
        { "balance" : (- balance amount) }
      )
    )
  )

  (defun credit
    ( accountId:string
      guard:guard
      amount:decimal )

    @doc " Increase an account balance. Internal use only. "

    @model [ (property (> amount 0.0))
             (property (valid-account-id accountId))
           ]

    (validate-account-id accountId)
    (enforce (> amount 0.0) "Credit amount must be positive.")
    (enforce-unit amount)
    (require-capability (CREDIT accountId))

    (with-default-read crankk01-token-table accountId
      { "balance" : -1.0, "guard" : guard }
      { "balance" := balance, "guard" := retg }
      ; we don't want to overwrite an existing guard with the user-supplied one
      (enforce (= retg guard)
        "account guards do not match")

      (let ((is-new
             (if (= balance -1.0)
                 (enforce-reserved accountId guard)
               false)))

        (write crankk01-token-table accountId
          { "balance" : (if is-new amount (+ balance amount))
          , "guard"   : retg
          }))
      ))

  (defun check-reserved:string (accountId:string)
    " Checks ACCOUNT for reserved name and returns type if \
    \ found or empty string. Reserved names start with a \
    \ single char and colon, e.g. 'c:foo', which would return 'c' as type."
    (let ((pfx (take 2 accountId)))
      (if (= ":" (take -1 pfx)) (take 1 pfx) "")))

  (defun enforce-reserved:bool (accountId:string guard:guard)
    @doc "Enforce reserved account name protocols."
    (let ((r (check-reserved accountId)))
      (if (= "" r) true
        (if (= "k" r)
          (enforce
            (= (format "{}" [guard])
               (format "KeySet {keys: [{}],pred: keys-all}"
                       [(drop 2 accountId)]))
            "Single-key account protocol violation")
          (enforce false
            (format "Unrecognized reserved protocol: {}" [r]))))))

  (defschema crosschain-schema
    @doc " Schema for yielded value in cross-chain transfers "
    receiver:string
    receiver-guard:guard
    amount:decimal
  )

  (defpact transfer-crosschain:string
    ( sender:string
      receiver:string
      receiver-guard:guard
      target-chain:string
      amount:decimal )

    @model [ (property (> amount 0.0))
             (property (!= receiver ""))
             (property (valid-account-id sender))
             (property (valid-account-id receiver))
           ]

    (step
      (with-capability (DEBIT sender)

        (validate-account-id sender)
        (validate-account-id receiver)

        (enforce (!= "" target-chain) "empty target-chain")
        (enforce (!= (at 'chain-id (chain-data)) target-chain)
          "cannot run cross-chain transfers to the same chain")

        (enforce (> amount 0.0)
          "transfer quantity must be positive")

        (enforce-unit amount)

        ;; Step 1 - debit sender account on current chain
        (debit sender amount)

        (let
          ((
            crosschain-details:object{crosschain-schema}
            { "receiver"       : receiver
            , "receiver-guard" : receiver-guard
            , "amount"         : amount
            }
          ))
          (yield crosschain-details target-chain)
        )
      )
    )

    (step
      (resume
        { "receiver"       := receiver
        , "receiver-guard" := receiver-guard
        , "amount"         := amount
        }
        ;; Step 2 - credit receiver account on target chain
        (with-capability (CREDIT receiver)
          (credit receiver receiver-guard amount)
        )
      )
    )
  )

  (defun get-balance:decimal
    ( account:string )

    (at 'balance (read crankk01-token-table account ['balance]))
  )

  (defun details:object{fungible-v2.account-details}
    ( account:string )

    (with-read crankk01-token-table account
      { "balance" := balance
      , "guard"   := guard
      }
      { "account" : account
      , "balance" : balance
      , "guard"   : guard
      }
    )
  )

  (defun precision:integer
    ()

    DECIMALS
  )

  (defun enforce-unit:bool
    ( amount:decimal )

    @doc " Enforce the minimum denomination for token transactions. "

    (enforce
      (= (floor amount DECIMALS) amount)
      (format "Amount violates minimum denomination: {}" [amount])
    )
  )

  (defun create-account:string
    ( account:string
      guard:guard )

    @doc " Create a new account. "

    @model [ (property (valid-account-id account)) ]

    (validate-account-id account)
    (enforce-reserved account guard)

    (insert crankk01-token-table account
      { "balance" : 0.0
      , "guard"   : guard
      }
    )
  )

  (defun rotate:string
    ( account:string
      new-guard:guard )

    (with-read crankk01-token-table account
      { "guard" := oldGuard }

      (enforce-guard oldGuard)
      (enforce-guard new-guard)

      (update crankk01-token-table account
        { "guard" : new-guard }
      )
    )
  )

  ;; ; --------------------------------------------------------------------------
  ;; ; Custom Functions

  (defun initialize:string
    ()

    @doc " Initialize the contract. \
         \ Admin-only. Should fail if it has been called before. "

    (with-capability (GOVERNANCE)
      (create-account ROOT_ACCOUNT_ID (create-module-guard "root-account"))
      (update crankk01-token-table ROOT_ACCOUNT_ID { "balance" : INITIAL_SUPPLY })
    )
  )
)

(create-table crankk01-token-table)


