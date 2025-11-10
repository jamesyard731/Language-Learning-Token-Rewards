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


(define-constant reviewer-eligibility-threshold u25)
(define-constant review-reward u100000)
(define-constant max-reviews-per-lesson u3)

(define-constant err-not-eligible-reviewer (err u300))
(define-constant err-cannot-review-own (err u301))
(define-constant err-already-reviewed (err u302))
(define-constant err-max-reviews-reached (err u303))
(define-constant err-lesson-not-completed (err u304))

(define-map peer-reviews
  { reviewer: principal, reviewee: principal, lesson-id: uint }
  { approved: bool, reviewed-at: uint, comments: (string-ascii 200) }
)

(define-map lesson-review-count
  { reviewee: principal, lesson-id: uint }
  { count: uint }
)

(define-map reviewer-stats
  { reviewer: principal }
  { total-reviews: uint, approvals: uint, rejections: uint }
)

(define-map user-reputation
  { user: principal }
  { total-reviews-received: uint, positive-reviews: uint, reputation-score: uint }
)

(define-read-only (is-eligible-reviewer (user principal))
  (let ((stats (get-user-stats user)))
    (>= (get total-completed stats) reviewer-eligibility-threshold)
  )
)

(define-read-only (get-review-stats (reviewer principal))
  (default-to
    { total-reviews: u0, approvals: u0, rejections: u0 }
    (map-get? reviewer-stats { reviewer: reviewer })
  )
)

(define-read-only (get-user-reputation (user principal))
  (default-to
    { total-reviews-received: u0, positive-reviews: u0, reputation-score: u0 }
    (map-get? user-reputation { user: user })
  )
)

(define-public (submit-peer-review (reviewee principal) (lesson-id uint) (approved bool) (comments (string-ascii 200)))
  (let ((reviewee-progress (get-user-progress reviewee lesson-id))
        (review-count-data (default-to { count: u0 } (map-get? lesson-review-count { reviewee: reviewee, lesson-id: lesson-id })))
        (reviewer-data (get-review-stats tx-sender))
        (reputation-data (get-user-reputation reviewee)))
    (asserts! (is-eligible-reviewer tx-sender) err-not-eligible-reviewer)
    (asserts! (not (is-eq tx-sender reviewee)) err-cannot-review-own)
    (asserts! (is-some reviewee-progress) err-lesson-not-completed)
    (asserts! (is-none (map-get? peer-reviews { reviewer: tx-sender, reviewee: reviewee, lesson-id: lesson-id })) err-already-reviewed)
    (asserts! (< (get count review-count-data) max-reviews-per-lesson) err-max-reviews-reached)
    (map-set peer-reviews
      { reviewer: tx-sender, reviewee: reviewee, lesson-id: lesson-id }
      { approved: approved, reviewed-at: stacks-block-height, comments: comments }
    )
    (map-set lesson-review-count
      { reviewee: reviewee, lesson-id: lesson-id }
      { count: (+ (get count review-count-data) u1) }
    )
    (map-set reviewer-stats
      { reviewer: tx-sender }
      { total-reviews: (+ (get total-reviews reviewer-data) u1),
        approvals: (if approved (+ (get approvals reviewer-data) u1) (get approvals reviewer-data)),
        rejections: (if approved (get rejections reviewer-data) (+ (get rejections reviewer-data) u1)) }
    )
    (let ((new-positive (if approved (+ (get positive-reviews reputation-data) u1) (get positive-reviews reputation-data)))
          (new-total (+ (get total-reviews-received reputation-data) u1)))
      (map-set user-reputation
        { user: reviewee }
        { total-reviews-received: new-total,
          positive-reviews: new-positive,
          reputation-score: (/ (* new-positive u100) new-total) }
      )
    )
    (try! (ft-mint? learn-token review-reward tx-sender))
    (ok { reviewed: reviewee, lesson: lesson-id, approved: approved })
  )
)


(define-constant referral-commission u100000)
(define-constant mentor-milestone-reward u250000)
(define-constant mentor-eligibility-lessons u30)
(define-constant max-mentees-per-mentor u10)

(define-constant err-invalid-referral (err u400))
(define-constant err-already-referred (err u401))
(define-constant err-not-eligible-mentor (err u402))
(define-constant err-max-mentees (err u403))
(define-constant err-already-has-mentor (err u404))

(define-map referral-codes
  { code: (string-ascii 20) }
  { referrer: principal, created-at: uint, active: bool }
)

(define-map user-referrals
  { user: principal }
  { referred-by: (optional principal), referral-code: (string-ascii 20), total-referrals: uint }
)

(define-map mentorships
  { mentor: principal, mentee: principal }
  { started-at: uint, mentee-lessons-at-start: uint, milestones-achieved: uint }
)

(define-map mentor-stats
  { mentor: principal }
  { total-mentees: uint, active-mentees: uint, total-earnings: uint }
)

(define-read-only (get-referral-info (user principal))
  (map-get? user-referrals { user: user })
)

(define-read-only (get-mentor-stats (mentor principal))
  (default-to
    { total-mentees: u0, active-mentees: u0, total-earnings: u0 }
    (map-get? mentor-stats { mentor: mentor })
  )
)

(define-public (create-referral-code (code (string-ascii 20)))
  (begin
    (asserts! (is-none (map-get? referral-codes { code: code })) err-invalid-referral)
    (map-set referral-codes
      { code: code }
      { referrer: tx-sender, created-at: stacks-block-height, active: true }
    )
    (ok code)
  )
)

(define-public (register-with-referral (code (string-ascii 20)))
  (let ((code-data (unwrap! (map-get? referral-codes { code: code }) err-invalid-referral))
        (existing-referral (map-get? user-referrals { user: tx-sender })))
    (asserts! (is-none existing-referral) err-already-referred)
    (asserts! (get active code-data) err-invalid-referral)
    (let ((referrer (get referrer code-data)))
      (asserts! (not (is-eq tx-sender referrer)) err-invalid-referral)
      (map-set user-referrals
        { user: tx-sender }
        { referred-by: (some referrer), referral-code: code, total-referrals: u0 }
      )
      (map-set user-referrals
        { user: referrer }
        (merge
          (default-to { referred-by: none, referral-code: "", total-referrals: u0 } (map-get? user-referrals { user: referrer }))
          { total-referrals: (+ (default-to u0 (get total-referrals (map-get? user-referrals { user: referrer }))) u1) }
        )
      )
      (ok { referrer: referrer, code: code })
    )
  )
)

(define-public (become-mentor (mentee principal))
  (let ((mentor-stats-data (get-mentor-stats tx-sender))
        (mentor-lessons (get total-completed (get-user-stats tx-sender)))
        (existing-mentorship (map-get? mentorships { mentor: tx-sender, mentee: mentee })))
    (asserts! (>= mentor-lessons mentor-eligibility-lessons) err-not-eligible-mentor)
    (asserts! (< (get active-mentees mentor-stats-data) max-mentees-per-mentor) err-max-mentees)
    (asserts! (not (is-eq tx-sender mentee)) err-invalid-referral)
    (asserts! (is-none existing-mentorship) err-already-has-mentor)
    (let ((mentee-stats (get-user-stats mentee)))
      (map-set mentorships
        { mentor: tx-sender, mentee: mentee }
        { started-at: stacks-block-height, mentee-lessons-at-start: (get total-completed mentee-stats), milestones-achieved: u0 }
      )
      (map-set mentor-stats
        { mentor: tx-sender }
        { total-mentees: (+ (get total-mentees mentor-stats-data) u1),
          active-mentees: (+ (get active-mentees mentor-stats-data) u1),
          total-earnings: (get total-earnings mentor-stats-data) }
      )
      (ok { mentor: tx-sender, mentee: mentee })
    )
  )
)