# 🎓 Language Learning Token Rewards

## 📖 Overview
A blockchain-powered language learning platform that rewards students with **LEARN tokens** for completing verified lessons. Built on Stacks blockchain using Clarity smart contracts.

## 🚀 Features
- 🏆 **Token Rewards**: Earn LEARN tokens for completing lessons with 70%+ score
- 📈 **Streak Bonuses**: Maintain daily streaks for 2x reward multipliers
- 📊 **Progress Tracking**: Monitor completion stats and learning progress
- 🎯 **Difficulty Scaling**: Higher difficulty lessons offer greater rewards
- 🔒 **Daily Limits**: Anti-spam protection with daily earning caps
- ✅ **Lesson Verification**: Score-based verification system

## 💰 Token Economics
- **Symbol**: LEARN
- **Decimals**: 6
- **Base Reward**: 1 LEARN token per lesson
- **Streak Bonus**: 2x multiplier after 7-day streak
- **Daily Cap**: 5 LEARN tokens maximum per day
- **Minimum Score**: 70% required for token rewards

## 🛠️ Contract Functions

### 📚 For Learners
```clarity
(complete-lesson lesson-id score)
```
Complete a lesson with your score (0-100). Earn tokens for scores ≥70%.

```clarity
(get-user-stats user)
```
View your learning statistics and progress.

```clarity
(get-user-progress user lesson-id)
```
Check completion status for specific lessons.

### 👩‍🏫 For Educators/Admins
```clarity
(create-lesson title difficulty reward-multiplier)
```
Create new lessons with custom difficulty and rewards.

```clarity
(toggle-lesson lesson-id)
```
Enable/disable lessons.

### 💸 Token Functions
```clarity
(transfer amount from to memo)
```
Transfer LEARN tokens between users.

```clarity
(get-balance user)
```
Check token balance.

## 🔧 Usage Instructions

### 📝 Creating Lessons
Only contract owner can create lessons:
```clarity
(contract-call? .language-learning-token-rewards create-lesson 
  "Spanish Basics" 
  u1 
  u1)
```

### 🎯 Completing Lessons
Students complete lessons and earn rewards:
```clarity
(contract-call? .language-learning-token-rewards complete-lesson 
  u1 
  u85)
```

### 📊 Checking Progress
View learning statistics:
```clarity
(contract-call? .language-learning-token-rewards get-user-stats 
  'SP1234567890ABCDEF)
```

## 🏗️ Development Setup

### Prerequisites
- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet for testing

### Installation
```bash
git clone https://github.com/your-repo/Language-Learning-Token-Rewards
cd Language-Learning-Token-Rewards
clarinet check
```

### Testing
```bash
clarinet test
```

### Deployment
```bash
clarinet integrate
```

## 🎮 Game Mechanics

### 🔥 Streak System
- Complete lessons daily to maintain streak
- 7+ day streaks earn 2x rewards
- Streaks reset after missing 2+ days

### 🎚️ Difficulty Levels
1. **Beginner** (1x multiplier)
2. **Elementary** (1.2x multiplier)
3. **Intermediate** (1.5x multiplier)
4. **Advanced** (2x multiplier)
5. **Expert** (3x multiplier)

### 🛡️ Anti-Abuse Features
- Daily earning caps prevent spam
- Score verification required for rewards
- Lesson completion tracking prevents duplicates

## 📈 Smart Contract Architecture

The contract implements:
- **SIP-010** fungible token standard
- **Lesson management** system
- **User progress tracking**
- **Reward calculation** engine
- **Streak mechanics**

## 🤝 Contributing
1. Fork the repository
2. Create feature branch
3. Add tests for new functionality
4. Submit pull request

## 📄 License
MIT License - see LICENSE file for details

---
**Happy Learning!** 🎉📚✨
