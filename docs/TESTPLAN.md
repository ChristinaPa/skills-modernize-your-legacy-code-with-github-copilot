# Student Account Management System - Test Plan

## Overview

This test plan documents the test cases for validating the business logic of the Student Account Management System. These test cases are designed to be used for stakeholder validation and will serve as the foundation for unit and integration tests in the Node.js migration.

---

## Test Environment

- **Original System:** COBOL Application
- **Target System:** Node.js Application
- **Test Data:** In-memory account balance (initial value: $1,000.00)

---

## Test Cases

### 1. Menu Navigation Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-MENU-001 | Display main menu on application start | Application is compiled and ready | 1. Start the application | Menu displays with options: 1. View Balance, 2. Credit Account, 3. Debit Account, 4. Exit | | | |
| TC-MENU-002 | Valid menu option - Option 1 | Application is running, menu is displayed | 1. Enter "1" at menu prompt | System proceeds to View Balance functionality | | | |
| TC-MENU-003 | Valid menu option - Option 2 | Application is running, menu is displayed | 1. Enter "2" at menu prompt | System proceeds to Credit Account functionality and prompts for amount | | | |
| TC-MENU-004 | Valid menu option - Option 3 | Application is running, menu is displayed | 1. Enter "3" at menu prompt | System proceeds to Debit Account functionality and prompts for amount | | | |
| TC-MENU-005 | Valid menu option - Option 4 (Exit) | Application is running, menu is displayed | 1. Enter "4" at menu prompt | System displays "Exiting the program. Goodbye!" and terminates | | | |
| TC-MENU-006 | Invalid menu option | Application is running, menu is displayed | 1. Enter "5" at menu prompt | System displays "Invalid choice, please select 1-4." and redisplays menu | | | |
| TC-MENU-007 | Invalid menu option - zero | Application is running, menu is displayed | 1. Enter "0" at menu prompt | System displays "Invalid choice, please select 1-4." and redisplays menu | | | |
| TC-MENU-008 | Menu loop continues after operation | Application is running, any operation completed | 1. Complete any valid operation (1, 2, or 3) | Menu is redisplayed for next action | | | |

---

### 2. View Balance Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-VIEW-001 | View initial balance | Application just started, no transactions performed | 1. Select option 1 (View Balance) | System displays "Current balance: 001000.00" | | | Initial balance is $1,000.00 |
| TC-VIEW-002 | View balance after credit | Balance has been credited | 1. Credit $500.00 to account 2. Select option 1 (View Balance) | System displays "Current balance: 001500.00" | | | |
| TC-VIEW-003 | View balance after debit | Balance has been debited | 1. Debit $300.00 from account 2. Select option 1 (View Balance) | System displays "Current balance: 000700.00" | | | |
| TC-VIEW-004 | View balance after multiple transactions | Multiple credits and debits performed | 1. Credit $500.00 2. Debit $200.00 3. Credit $100.00 4. Select option 1 | System displays "Current balance: 001400.00" | | | Tests cumulative balance |
| TC-VIEW-005 | View zero balance | Balance has been fully debited | 1. Debit $1000.00 (full balance) 2. Select option 1 | System displays "Current balance: 000000.00" | | | |

---

### 3. Credit Account Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-CREDIT-001 | Credit whole number amount | Application running, initial balance $1,000.00 | 1. Select option 2 2. Enter amount: 500 | System displays "Amount credited. New balance: 001500.00" | | | |
| TC-CREDIT-002 | Credit decimal amount | Application running, initial balance $1,000.00 | 1. Select option 2 2. Enter amount: 250.50 | System displays "Amount credited. New balance: 001250.50" | | | |
| TC-CREDIT-003 | Credit small amount | Application running, initial balance $1,000.00 | 1. Select option 2 2. Enter amount: 0.01 | System displays "Amount credited. New balance: 001000.01" | | | Minimum credit amount |
| TC-CREDIT-004 | Credit large amount | Application running, initial balance $1,000.00 | 1. Select option 2 2. Enter amount: 998999.99 | System displays "Amount credited. New balance: 999999.99" | | | Maximum possible balance |
| TC-CREDIT-005 | Credit zero amount | Application running, initial balance $1,000.00 | 1. Select option 2 2. Enter amount: 0 | System displays "Amount credited. New balance: 001000.00" | | | Balance unchanged |
| TC-CREDIT-006 | Multiple sequential credits | Application running, initial balance $1,000.00 | 1. Credit $100.00 2. Credit $200.00 3. Credit $300.00 | Final balance: $1,600.00 | | | Tests cumulative credits |
| TC-CREDIT-007 | Credit after debit | Balance reduced by previous debit | 1. Debit $500.00 2. Credit $300.00 | System displays "Amount credited. New balance: 000800.00" | | | |

---

### 4. Debit Account Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-DEBIT-001 | Debit whole number amount with sufficient funds | Application running, balance $1,000.00 | 1. Select option 3 2. Enter amount: 500 | System displays "Amount debited. New balance: 000500.00" | | | |
| TC-DEBIT-002 | Debit decimal amount with sufficient funds | Application running, balance $1,000.00 | 1. Select option 3 2. Enter amount: 250.50 | System displays "Amount debited. New balance: 000749.50" | | | |
| TC-DEBIT-003 | Debit exact balance amount | Application running, balance $1,000.00 | 1. Select option 3 2. Enter amount: 1000 | System displays "Amount debited. New balance: 000000.00" | | | Boundary: balance = amount |
| TC-DEBIT-004 | Debit more than balance (insufficient funds) | Application running, balance $1,000.00 | 1. Select option 3 2. Enter amount: 1500 | System displays "Insufficient funds for this debit." Balance remains $1,000.00 | | | **Critical business rule** |
| TC-DEBIT-005 | Debit small amount | Application running, balance $1,000.00 | 1. Select option 3 2. Enter amount: 0.01 | System displays "Amount debited. New balance: 000999.99" | | | Minimum debit amount |
| TC-DEBIT-006 | Debit zero amount | Application running, balance $1,000.00 | 1. Select option 3 2. Enter amount: 0 | System displays "Amount debited. New balance: 001000.00" | | | Balance unchanged |
| TC-DEBIT-007 | Multiple sequential debits with sufficient funds | Application running, balance $1,000.00 | 1. Debit $100.00 2. Debit $200.00 3. Debit $300.00 | Final balance: $400.00 | | | Tests cumulative debits |
| TC-DEBIT-008 | Debit after previous insufficient funds attempt | Previous debit rejected | 1. Attempt debit $1500 (rejected) 2. Debit $500 | First attempt rejected, second succeeds with balance $500.00 | | | Balance not affected by failed attempt |
| TC-DEBIT-009 | Debit $1 more than balance | Application running, balance $1,000.00 | 1. Select option 3 2. Enter amount: 1000.01 | System displays "Insufficient funds for this debit." Balance remains $1,000.00 | | | Boundary: amount just exceeds balance |

---

### 5. Data Persistence Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-DATA-001 | Balance persists across operations | Application running | 1. Credit $500 2. View Balance 3. Debit $200 4. View Balance | Balance correctly reflects all operations ($1,300.00) | | | |
| TC-DATA-002 | Balance read operation returns current value | Balance modified | 1. Perform multiple transactions 2. View balance | Displayed balance matches expected calculated value | | | |
| TC-DATA-003 | Balance write operation updates storage | Credit performed | 1. Credit $100 2. View balance | New balance persisted and correctly displayed | | | |

---

### 6. Integration Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-INT-001 | Full transaction workflow | Application just started | 1. View Balance ($1,000) 2. Credit $500 ($1,500) 3. Debit $300 ($1,200) 4. View Balance 5. Exit | All operations complete successfully, final balance $1,200.00, clean exit | | | End-to-end workflow |
| TC-INT-002 | Mixed valid and invalid operations | Application just started | 1. Enter invalid menu option 2. View Balance 3. Attempt debit > balance 4. Credit $100 5. Exit | Invalid operations handled gracefully, valid operations succeed | | | Error handling |
| TC-INT-003 | Debit to zero then credit | Application just started | 1. Debit $1,000 (balance $0) 2. Credit $500 3. View Balance | Balance correctly shows $500.00 | | | Recovery from zero balance |
| TC-INT-004 | Repeated menu usage | Application just started | 1. Perform 10 sequential operations 2. Exit | All operations processed correctly, menu loops as expected | | | Stress test menu loop |

---

### 7. Boundary Value Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-BND-001 | Maximum balance value | Balance near maximum | 1. Set balance to $999,999.99 via credits 2. View Balance | System displays maximum value correctly | | | PIC 9(6)V99 max = 999999.99 |
| TC-BND-002 | Minimum balance value (zero) | Balance is $1,000.00 | 1. Debit $1,000.00 2. View Balance | System displays "Current balance: 000000.00" | | | |
| TC-BND-003 | Two decimal precision - credit | Balance is $1,000.00 | 1. Credit $123.45 | Balance shows $1,123.45 with correct precision | | | |
| TC-BND-004 | Two decimal precision - debit | Balance is $1,000.00 | 1. Debit $123.45 | Balance shows $876.55 with correct precision | | | |

---

## Business Rules Summary

The following business rules must be validated by all tests:

| Rule ID | Business Rule | Related Test Cases |
|---------|--------------|-------------------|
| BR-001 | Initial account balance is $1,000.00 | TC-VIEW-001 |
| BR-002 | Maximum account balance is $999,999.99 | TC-BND-001, TC-CREDIT-004 |
| BR-003 | Minimum account balance is $0.00 (no negative balances) | TC-DEBIT-004, TC-DEBIT-009 |
| BR-004 | Debit transactions are rejected if amount exceeds current balance | TC-DEBIT-004, TC-DEBIT-009 |
| BR-005 | All monetary values have 2 decimal place precision | TC-BND-003, TC-BND-004 |
| BR-006 | Credit operations have no upper limit per transaction | TC-CREDIT-004 |
| BR-007 | Failed debit attempts do not modify the balance | TC-DEBIT-008 |
| BR-008 | Menu options must be 1-4, other values are rejected | TC-MENU-006, TC-MENU-007 |

---

## Test Execution Sign-Off

| Role | Name | Signature | Date |
|------|------|-----------|------|
| Business Stakeholder | | | |
| QA Lead | | | |
| Developer | | | |
| Project Manager | | | |

---

## Notes for Node.js Implementation

When implementing these tests in Node.js:

1. **Unit Tests:** Focus on individual functions (viewBalance, creditAccount, debitAccount)
2. **Integration Tests:** Test the interaction between modules (menu → operations → data)
3. **Use Jest or Mocha:** Recommended testing frameworks for Node.js
4. **Mock the data layer:** For isolated unit testing of operations
5. **Test the insufficient funds logic:** Critical business rule that must be preserved
6. **Ensure decimal precision:** JavaScript floating-point considerations may require special handling (consider using decimal.js or similar)
