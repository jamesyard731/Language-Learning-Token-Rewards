(define-trait sip010-trait
  (
    (transfer (uint principal principal (optional (buff 34))) (response bool uint))
    (get-name () (response (string-ascii 32) uint))
    (get-symbol () (response (string-ascii 10) uint))
    (get-decimals () (response uint uint))
    (get-balance (principal) (response uint uint))
    (get-total-supply () (response uint uint))
    (get-token-uri ((optional uint)) (response (optional (string-utf8 256)) uint))
  )
)

(define-fungible-token learn-token)

(define-constant contract-owner tx-sender)
(define-constant token-name "Learn Token")
(define-constant token-symbol "LEARN")
(define-constant token-decimals u6)
(define-constant lesson-reward u1000000)
(define-constant streak-bonus u500000)
(define-constant daily-cap u5000000)

(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-lesson-not-found (err u102))
(define-constant err-lesson-already-completed (err u103))
(define-constant err-daily-cap-reached (err u104))
(define-constant err-invalid-lesson (err u105))


(define-constant rookie-threshold u10)
(define-constant streak-threshold u7)
(define-constant marathon-threshold u50)

(define-constant err-invalid-badge (err u200))
(define-constant err-already-claimed (err u201))
(define-constant err-not-eligible (err u202))

(define-data-var next-badge-token-id uint u1)

(define-data-var total-lessons uint u0)
(define-data-var next-lesson-id uint u1)

(define-map lessons
  { lesson-id: uint }
  {
    title: (string-ascii 100),
    difficulty: uint,
    reward-multiplier: uint,
    active: bool,
    created-by: principal
  }
)

(define-map user-progress
  { user: principal, lesson-id: uint }
  {
    completed-at: uint,
    score: uint,
    verified: bool
  }
)

(define-map user-stats
  { user: principal }
  {
    total-completed: uint,
    current-streak: uint,
    last-completion: uint,
    daily-earned: uint,
    last-reset-day: uint
  }
)

(define-map daily-earnings
  { user: principal, day: uint }
  { amount: uint }
)

(define-read-only (get-name)
  (ok token-name)
)

(define-read-only (get-symbol)
  (ok token-symbol)
)

(define-read-only (get-decimals)
  (ok token-decimals)
)

(define-read-only (get-balance (user principal))
  (ok (ft-get-balance learn-token user))
)

(define-read-only (get-total-supply)
  (ok (ft-get-supply learn-token))
)

(define-read-only (get-token-uri (token-id (optional uint)))
  (ok none)
)

(define-read-only (get-lesson (lesson-id uint))
  (map-get? lessons { lesson-id: lesson-id })
)

(define-read-only (get-user-progress (user principal) (lesson-id uint))
  (map-get? user-progress { user: user, lesson-id: lesson-id })
)

(define-read-only (get-user-stats (user principal))
  (default-to
    { total-completed: u0, current-streak: u0, last-completion: u0, daily-earned: u0, last-reset-day: u0 }
    (map-get? user-stats { user: user })
  )
)

(define-read-only (get-current-day)
  (/ stacks-block-height u144)
)

(define-private (is-new-day (last-day uint))
  (> (get-current-day) last-day)
)

(define-private (calculate-reward (lesson-id uint) (user principal))
  (let ((lesson (unwrap! (get-lesson lesson-id) u0))
        (stats (get-user-stats user)))
    (let ((base-reward (* lesson-reward (get reward-multiplier lesson)))
          (streak-multiplier (if (> (get current-streak stats) u6) u2 u1)))
      (/ (* base-reward streak-multiplier) u1)
    )
  )
)

(define-private (update-user-streak (user principal))
  (let ((stats (get-user-stats user))
        (current-time stacks-block-height)
        (last-completion (get last-completion stats)))
    (let ((time-diff (- current-time last-completion)))
      (if (and (> time-diff u144) (<= time-diff u288))
        (+ (get current-streak stats) u1)
        (if (> time-diff u288) u1 (get current-streak stats))
      )
    )
  )
)

(define-public (create-lesson (title (string-ascii 100)) (difficulty uint) (reward-multiplier uint))
  (let ((lesson-id (var-get next-lesson-id)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= difficulty u5) err-invalid-lesson)
    (asserts! (> reward-multiplier u0) err-invalid-lesson)
    (map-set lessons
      { lesson-id: lesson-id }
      {
        title: title,
        difficulty: difficulty,
        reward-multiplier: reward-multiplier,
        active: true,
        created-by: tx-sender
      }
    )
    (var-set next-lesson-id (+ lesson-id u1))
    (var-set total-lessons (+ (var-get total-lessons) u1))
    (ok lesson-id)
  )
)

(define-public (complete-lesson (lesson-id uint) (score uint))
  (let ((lesson (unwrap! (get-lesson lesson-id) err-lesson-not-found))
        (stats (get-user-stats tx-sender))
        (current-day (get-current-day))
        (progress (get-user-progress tx-sender lesson-id)))
    (asserts! (get active lesson) err-lesson-not-found)
    (asserts! (is-none progress) err-lesson-already-completed)
    (asserts! (<= score u100) err-invalid-lesson)
    (let ((daily-earned (if (is-new-day (get last-reset-day stats)) u0 (get daily-earned stats))))
      (asserts! (< daily-earned daily-cap) err-daily-cap-reached)
      (let ((reward (calculate-reward lesson-id tx-sender))
            (new-streak (update-user-streak tx-sender))
            (verified (>= score u70)))
        (map-set user-progress
          { user: tx-sender, lesson-id: lesson-id }
          {
            completed-at: stacks-block-height,
            score: score,
            verified: verified
          }
        )
        (map-set user-stats
          { user: tx-sender }
          {
            total-completed: (+ (get total-completed stats) u1),
            current-streak: new-streak,
            last-completion: stacks-block-height,
            daily-earned: (+ daily-earned reward),
            last-reset-day: current-day
          }
        )
        (if verified
          (begin
            (try! (ft-mint? learn-token reward tx-sender))
            (ok { reward: reward, verified: true })
          )
          (ok { reward: u0, verified: false })
        )
      )
    )
  )
)

(define-public (transfer (amount uint) (from principal) (to principal) (memo (optional (buff 34))))
  (begin
    (asserts! (or (is-eq tx-sender from) (is-eq contract-caller from)) err-not-authorized)
    (ft-transfer? learn-token amount from to)
  )
)

(define-public (toggle-lesson (lesson-id uint))
  (let ((lesson (unwrap! (get-lesson lesson-id) err-lesson-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set lessons
      { lesson-id: lesson-id }
      (merge lesson { active: (not (get active lesson)) })
    )
    (ok true)
  )
)

(define-public (mint-admin (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ft-mint? learn-token amount recipient)
  )
)


(define-non-fungible-token learn-badge uint)

(define-map user-badges
  { user: principal, badge-id: uint }
  { claimed: bool, token-id: uint }
)

(define-map badge-metadata
  { badge-id: uint }
  {
    name: (string-ascii 50),
    description: (string-ascii 150),
    icon: (string-ascii 10)
  }
)

(define-read-only (get-badge-info (badge-id uint))
  (map-get? badge-metadata { badge-id: badge-id })
)

(define-read-only (has-badge (user principal) (badge-id uint))
  (is-some (map-get? user-badges { user: user, badge-id: badge-id }))
)

(define-read-only (get-user-badge (user principal) (badge-id uint))
  (map-get? user-badges { user: user, badge-id: badge-id })
)

(define-private (is-eligible-for-badge (user principal) (badge-id uint))
  (let ((stats (get-user-stats user)))
    (if (is-eq badge-id u0)
        (>= (get total-completed stats) rookie-threshold)
        (if (is-eq badge-id u1)
            (>= (get current-streak stats) streak-threshold)
            (if (is-eq badge-id u2)
                (>= (get total-completed stats) marathon-threshold)
                false
            )
        )
    )
  )
)

(define-public (claim-badge (badge-id uint))
  (let ((token-id (var-get next-badge-token-id)))
    (asserts! (<= badge-id u2) err-invalid-badge)
    (asserts! (not (has-badge tx-sender badge-id)) err-already-claimed)
    (asserts! (is-eligible-for-badge tx-sender badge-id) err-not-eligible)
    (map-set user-badges
      { user: tx-sender, badge-id: badge-id }
      { claimed: true, token-id: token-id }
    )
    (var-set next-badge-token-id (+ token-id u1))
    (try! (nft-mint? learn-badge token-id tx-sender))
    (ok { badge-id: badge-id, token-id: token-id })
  )
)

(map-set badge-metadata { badge-id: u0 }
  { name: "Rookie Scholar", description: "Complete 10 lessons to earn your first badge", icon: "ROOKIE" })
(map-set badge-metadata { badge-id: u1 }
  { name: "Streak Master", description: "Maintain a 7-day learning streak", icon: "STREAK" })
(map-set badge-metadata { badge-id: u2 }
  { name: "Marathon Learner", description: "Complete 50 lessons to prove your dedication", icon: "MARATHON" })
