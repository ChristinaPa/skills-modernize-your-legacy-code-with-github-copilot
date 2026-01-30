/**
 * Student Account Management System - Unit Tests
 * 
 * These tests mirror the scenarios defined in docs/TESTPLAN.md
 * to validate the business logic migrated from the COBOL application.
 * 
 * Test Categories:
 * 1. View Balance Tests (TC-VIEW-xxx)
 * 2. Credit Account Tests (TC-CREDIT-xxx)
 * 3. Debit Account Tests (TC-DEBIT-xxx)
 * 4. Data Persistence Tests (TC-DATA-xxx)
 * 5. Integration Tests (TC-INT-xxx)
 * 6. Boundary Value Tests (TC-BND-xxx)
 */

const { DataProgram, Operations } = require('./index');

// Suppress console.log during tests
beforeAll(() => {
    jest.spyOn(console, 'log').mockImplementation(() => {});
});

afterAll(() => {
    console.log.mockRestore();
});

// Reset balance before each test to ensure test isolation
beforeEach(() => {
    DataProgram.reset();
});

// ============================================================================
// 2. VIEW BALANCE TESTS
// ============================================================================

describe('View Balance Tests', () => {
    
    test('TC-VIEW-001: View initial balance', () => {
        // Pre-conditions: Application just started, no transactions performed
        // Expected: System displays "Current balance: 001000.00"
        const balance = Operations.getBalance();
        expect(balance).toBe(1000.00);
    });

    test('TC-VIEW-002: View balance after credit', () => {
        // Pre-conditions: Balance has been credited
        // Test Steps: 1. Credit $500.00 2. View Balance
        // Expected: Balance is $1,500.00
        Operations.credit(500.00);
        const balance = Operations.getBalance();
        expect(balance).toBe(1500.00);
    });

    test('TC-VIEW-003: View balance after debit', () => {
        // Pre-conditions: Balance has been debited
        // Test Steps: 1. Debit $300.00 2. View Balance
        // Expected: Balance is $700.00
        Operations.debit(300.00);
        const balance = Operations.getBalance();
        expect(balance).toBe(700.00);
    });

    test('TC-VIEW-004: View balance after multiple transactions', () => {
        // Pre-conditions: Multiple credits and debits performed
        // Test Steps: 1. Credit $500 2. Debit $200 3. Credit $100 4. View Balance
        // Expected: Balance is $1,400.00
        Operations.credit(500.00);
        Operations.debit(200.00);
        Operations.credit(100.00);
        const balance = Operations.getBalance();
        expect(balance).toBe(1400.00);
    });

    test('TC-VIEW-005: View zero balance', () => {
        // Pre-conditions: Balance has been fully debited
        // Test Steps: 1. Debit $1000.00 2. View Balance
        // Expected: Balance is $0.00
        Operations.debit(1000.00);
        const balance = Operations.getBalance();
        expect(balance).toBe(0.00);
    });
});

// ============================================================================
// 3. CREDIT ACCOUNT TESTS
// ============================================================================

describe('Credit Account Tests', () => {
    
    test('TC-CREDIT-001: Credit whole number amount', () => {
        // Pre-conditions: Initial balance $1,000.00
        // Test Steps: Credit 500
        // Expected: New balance $1,500.00
        const newBalance = Operations.credit(500);
        expect(newBalance).toBe(1500.00);
    });

    test('TC-CREDIT-002: Credit decimal amount', () => {
        // Pre-conditions: Initial balance $1,000.00
        // Test Steps: Credit 250.50
        // Expected: New balance $1,250.50
        const newBalance = Operations.credit(250.50);
        expect(newBalance).toBe(1250.50);
    });

    test('TC-CREDIT-003: Credit small amount', () => {
        // Pre-conditions: Initial balance $1,000.00
        // Test Steps: Credit 0.01
        // Expected: New balance $1,000.01
        const newBalance = Operations.credit(0.01);
        expect(newBalance).toBeCloseTo(1000.01, 2);
    });

    test('TC-CREDIT-004: Credit large amount', () => {
        // Pre-conditions: Initial balance $1,000.00
        // Test Steps: Credit 998999.99
        // Expected: New balance $999,999.99 (maximum balance)
        const newBalance = Operations.credit(998999.99);
        expect(newBalance).toBeCloseTo(999999.99, 2);
    });

    test('TC-CREDIT-005: Credit zero amount', () => {
        // Pre-conditions: Initial balance $1,000.00
        // Test Steps: Credit 0
        // Expected: Balance unchanged at $1,000.00
        const newBalance = Operations.credit(0);
        expect(newBalance).toBe(1000.00);
    });

    test('TC-CREDIT-006: Multiple sequential credits', () => {
        // Pre-conditions: Initial balance $1,000.00
        // Test Steps: 1. Credit $100 2. Credit $200 3. Credit $300
        // Expected: Final balance $1,600.00
        Operations.credit(100.00);
        Operations.credit(200.00);
        const finalBalance = Operations.credit(300.00);
        expect(finalBalance).toBe(1600.00);
    });

    test('TC-CREDIT-007: Credit after debit', () => {
        // Pre-conditions: Balance reduced by previous debit
        // Test Steps: 1. Debit $500 2. Credit $300
        // Expected: New balance $800.00
        Operations.debit(500.00);
        const newBalance = Operations.credit(300.00);
        expect(newBalance).toBe(800.00);
    });
});

// ============================================================================
// 4. DEBIT ACCOUNT TESTS
// ============================================================================

describe('Debit Account Tests', () => {
    
    test('TC-DEBIT-001: Debit whole number amount with sufficient funds', () => {
        // Pre-conditions: Balance $1,000.00
        // Test Steps: Debit 500
        // Expected: New balance $500.00
        const result = Operations.debit(500);
        expect(result.success).toBe(true);
        expect(result.balance).toBe(500.00);
    });

    test('TC-DEBIT-002: Debit decimal amount with sufficient funds', () => {
        // Pre-conditions: Balance $1,000.00
        // Test Steps: Debit 250.50
        // Expected: New balance $749.50
        const result = Operations.debit(250.50);
        expect(result.success).toBe(true);
        expect(result.balance).toBe(749.50);
    });

    test('TC-DEBIT-003: Debit exact balance amount (boundary)', () => {
        // Pre-conditions: Balance $1,000.00
        // Test Steps: Debit 1000
        // Expected: New balance $0.00
        const result = Operations.debit(1000);
        expect(result.success).toBe(true);
        expect(result.balance).toBe(0.00);
    });

    test('TC-DEBIT-004: Debit more than balance - insufficient funds (CRITICAL)', () => {
        // Pre-conditions: Balance $1,000.00
        // Test Steps: Debit 1500
        // Expected: "Insufficient funds for this debit." Balance remains $1,000.00
        const result = Operations.debit(1500);
        expect(result.success).toBe(false);
        expect(result.message).toBe('Insufficient funds for this debit.');
        expect(Operations.getBalance()).toBe(1000.00);
    });

    test('TC-DEBIT-005: Debit small amount', () => {
        // Pre-conditions: Balance $1,000.00
        // Test Steps: Debit 0.01
        // Expected: New balance $999.99
        const result = Operations.debit(0.01);
        expect(result.success).toBe(true);
        expect(result.balance).toBeCloseTo(999.99, 2);
    });

    test('TC-DEBIT-006: Debit zero amount', () => {
        // Pre-conditions: Balance $1,000.00
        // Test Steps: Debit 0
        // Expected: Balance unchanged at $1,000.00
        const result = Operations.debit(0);
        expect(result.success).toBe(true);
        expect(result.balance).toBe(1000.00);
    });

    test('TC-DEBIT-007: Multiple sequential debits with sufficient funds', () => {
        // Pre-conditions: Balance $1,000.00
        // Test Steps: 1. Debit $100 2. Debit $200 3. Debit $300
        // Expected: Final balance $400.00
        Operations.debit(100.00);
        Operations.debit(200.00);
        const result = Operations.debit(300.00);
        expect(result.success).toBe(true);
        expect(result.balance).toBe(400.00);
    });

    test('TC-DEBIT-008: Debit after previous insufficient funds attempt', () => {
        // Pre-conditions: Previous debit rejected
        // Test Steps: 1. Attempt debit $1500 (rejected) 2. Debit $500
        // Expected: First rejected, second succeeds with balance $500.00
        const firstResult = Operations.debit(1500);
        expect(firstResult.success).toBe(false);
        expect(Operations.getBalance()).toBe(1000.00);  // Balance not affected
        
        const secondResult = Operations.debit(500);
        expect(secondResult.success).toBe(true);
        expect(secondResult.balance).toBe(500.00);
    });

    test('TC-DEBIT-009: Debit $0.01 more than balance (boundary)', () => {
        // Pre-conditions: Balance $1,000.00
        // Test Steps: Debit 1000.01
        // Expected: "Insufficient funds for this debit." Balance remains $1,000.00
        const result = Operations.debit(1000.01);
        expect(result.success).toBe(false);
        expect(result.message).toBe('Insufficient funds for this debit.');
        expect(Operations.getBalance()).toBe(1000.00);
    });
});

// ============================================================================
// 5. DATA PERSISTENCE TESTS
// ============================================================================

describe('Data Persistence Tests', () => {
    
    test('TC-DATA-001: Balance persists across operations', () => {
        // Pre-conditions: Application running
        // Test Steps: 1. Credit $500 2. View Balance 3. Debit $200 4. View Balance
        // Expected: Balance correctly reflects all operations ($1,300.00)
        Operations.credit(500);
        expect(Operations.getBalance()).toBe(1500.00);
        
        Operations.debit(200);
        expect(Operations.getBalance()).toBe(1300.00);
    });

    test('TC-DATA-002: Balance read operation returns current value', () => {
        // Pre-conditions: Balance modified
        // Test Steps: Perform transactions and read balance
        // Expected: Displayed balance matches expected calculated value
        Operations.credit(123.45);
        Operations.debit(23.45);
        
        const balance = DataProgram.execute('READ');
        expect(balance).toBe(1100.00);
    });

    test('TC-DATA-003: Balance write operation updates storage', () => {
        // Pre-conditions: Credit performed
        // Test Steps: 1. Credit $100 2. Read balance
        // Expected: New balance persisted and correctly returned
        Operations.credit(100);
        
        const storedBalance = DataProgram.execute('READ');
        expect(storedBalance).toBe(1100.00);
    });

    test('DataProgram reset restores initial balance', () => {
        // Additional test for reset functionality
        Operations.credit(500);
        expect(Operations.getBalance()).toBe(1500.00);
        
        DataProgram.reset();
        expect(Operations.getBalance()).toBe(1000.00);
    });

    test('DataProgram getInitialBalance returns correct value', () => {
        // Test initial balance constant
        expect(DataProgram.getInitialBalance()).toBe(1000.00);
    });
});

// ============================================================================
// 6. INTEGRATION TESTS
// ============================================================================

describe('Integration Tests', () => {
    
    test('TC-INT-001: Full transaction workflow', () => {
        // Pre-conditions: Application just started
        // Test Steps: 1. View Balance ($1,000) 2. Credit $500 ($1,500) 
        //             3. Debit $300 ($1,200) 4. View Balance
        // Expected: Final balance $1,200.00
        expect(Operations.getBalance()).toBe(1000.00);
        
        Operations.credit(500);
        expect(Operations.getBalance()).toBe(1500.00);
        
        Operations.debit(300);
        expect(Operations.getBalance()).toBe(1200.00);
        
        const finalBalance = Operations.viewBalance();
        expect(finalBalance).toBe(1200.00);
    });

    test('TC-INT-002: Mixed valid and invalid operations', () => {
        // Pre-conditions: Application just started
        // Test Steps: 1. View Balance 2. Attempt debit > balance 3. Credit $100
        // Expected: Invalid operations handled gracefully, valid operations succeed
        expect(Operations.getBalance()).toBe(1000.00);
        
        const failedDebit = Operations.debit(2000);
        expect(failedDebit.success).toBe(false);
        expect(Operations.getBalance()).toBe(1000.00);  // Unchanged
        
        Operations.credit(100);
        expect(Operations.getBalance()).toBe(1100.00);
    });

    test('TC-INT-003: Debit to zero then credit', () => {
        // Pre-conditions: Application just started
        // Test Steps: 1. Debit $1,000 (balance $0) 2. Credit $500 3. View Balance
        // Expected: Balance correctly shows $500.00
        Operations.debit(1000);
        expect(Operations.getBalance()).toBe(0.00);
        
        Operations.credit(500);
        expect(Operations.getBalance()).toBe(500.00);
    });

    test('TC-INT-004: Repeated operations', () => {
        // Pre-conditions: Application just started
        // Test Steps: Perform 10 sequential operations
        // Expected: All operations processed correctly
        for (let i = 0; i < 5; i++) {
            Operations.credit(100);
        }
        expect(Operations.getBalance()).toBe(1500.00);
        
        for (let i = 0; i < 5; i++) {
            Operations.debit(50);
        }
        expect(Operations.getBalance()).toBe(1250.00);
    });
});

// ============================================================================
// 7. BOUNDARY VALUE TESTS
// ============================================================================

describe('Boundary Value Tests', () => {
    
    test('TC-BND-001: Maximum balance value', () => {
        // Pre-conditions: Balance near maximum
        // Test Steps: Set balance to $999,999.99 via credits
        // Expected: System handles maximum value correctly
        DataProgram.execute('WRITE', 999999.99);
        const balance = Operations.getBalance();
        expect(balance).toBe(999999.99);
    });

    test('TC-BND-002: Minimum balance value (zero)', () => {
        // Pre-conditions: Balance is $1,000.00
        // Test Steps: Debit $1,000.00
        // Expected: Balance is $0.00
        Operations.debit(1000.00);
        const balance = Operations.getBalance();
        expect(balance).toBe(0.00);
    });

    test('TC-BND-003: Two decimal precision - credit', () => {
        // Pre-conditions: Balance is $1,000.00
        // Test Steps: Credit $123.45
        // Expected: Balance shows $1,123.45 with correct precision
        Operations.credit(123.45);
        const balance = Operations.getBalance();
        expect(balance).toBe(1123.45);
    });

    test('TC-BND-004: Two decimal precision - debit', () => {
        // Pre-conditions: Balance is $1,000.00
        // Test Steps: Debit $123.45
        // Expected: Balance shows $876.55 with correct precision
        Operations.debit(123.45);
        const balance = Operations.getBalance();
        expect(balance).toBe(876.55);
    });

    test('Format balance matches COBOL PIC 9(6)V99 format', () => {
        // Test that formatBalance produces correct output
        expect(Operations.formatBalance(1000.00)).toBe('001000.00');
        expect(Operations.formatBalance(0.00)).toBe('000000.00');
        expect(Operations.formatBalance(999999.99)).toBe('999999.99');
        expect(Operations.formatBalance(123.45)).toBe('000123.45');
    });
});

// ============================================================================
// BUSINESS RULES VALIDATION
// ============================================================================

describe('Business Rules Validation', () => {
    
    test('BR-001: Initial account balance is $1,000.00', () => {
        expect(DataProgram.getInitialBalance()).toBe(1000.00);
        expect(Operations.getBalance()).toBe(1000.00);
    });

    test('BR-002: Maximum account balance is $999,999.99', () => {
        DataProgram.execute('WRITE', 999999.99);
        expect(Operations.getBalance()).toBe(999999.99);
    });

    test('BR-003: Minimum account balance is $0.00 (no negative balances)', () => {
        Operations.debit(1000.00);
        const result = Operations.debit(0.01);
        expect(result.success).toBe(false);
        expect(Operations.getBalance()).toBe(0.00);
    });

    test('BR-004: Debit transactions are rejected if amount exceeds current balance', () => {
        const result = Operations.debit(1500.00);
        expect(result.success).toBe(false);
        expect(Operations.getBalance()).toBe(1000.00);
    });

    test('BR-005: All monetary values have 2 decimal place precision', () => {
        Operations.credit(0.01);
        expect(Operations.getBalance()).toBeCloseTo(1000.01, 2);
        
        Operations.debit(0.01);
        expect(Operations.getBalance()).toBeCloseTo(1000.00, 2);
    });

    test('BR-006: Credit operations have no upper limit per transaction', () => {
        const newBalance = Operations.credit(998999.99);
        expect(newBalance).toBeCloseTo(999999.99, 2);
    });

    test('BR-007: Failed debit attempts do not modify the balance', () => {
        const initialBalance = Operations.getBalance();
        Operations.debit(2000.00);
        expect(Operations.getBalance()).toBe(initialBalance);
    });
});
