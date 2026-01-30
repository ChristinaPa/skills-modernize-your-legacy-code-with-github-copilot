/**
 * Student Account Management System
 * Migrated from COBOL to Node.js
 * 
 * Original COBOL modules:
 * - main.cob      -> MainProgram (menu and main loop)
 * - operations.cob -> Operations (business logic)
 * - data.cob      -> DataProgram (data storage layer)
 * 
 * Business Rules Preserved:
 * - Initial balance: $1,000.00
 * - Maximum balance: $999,999.99 (PIC 9(6)V99)
 * - Minimum balance: $0.00 (no negative balances)
 * - Debit rejected if amount exceeds current balance
 * - All monetary values have 2 decimal precision
 */

const readlineSync = require('readline-sync');

// Check if running as main module or being required for testing
const isMainModule = require.main === module;

// ============================================================================
// DATA MODULE (Equivalent to data.cob - DataProgram)
// ============================================================================

/**
 * DataProgram - Data storage layer
 * Simulates persistent storage using module-level variable
 */
const DataProgram = (() => {
    // WORKING-STORAGE SECTION
    // 01 STORAGE-BALANCE PIC 9(6)V99 VALUE 1000.00
    const INITIAL_BALANCE = 1000.00;
    let storageBalance = INITIAL_BALANCE;

    /**
     * Perform data operations (READ or WRITE)
     * @param {string} operation - 'READ' or 'WRITE'
     * @param {number} balance - Balance value (used for WRITE, ignored for READ)
     * @returns {number} Current balance for READ operations
     */
    function execute(operation, balance = 0) {
        if (operation === 'READ') {
            return storageBalance;
        } else if (operation === 'WRITE') {
            storageBalance = balance;
            return storageBalance;
        }
        return storageBalance;
    }

    /**
     * Reset balance to initial value (for testing purposes)
     */
    function reset() {
        storageBalance = INITIAL_BALANCE;
    }

    /**
     * Get the initial balance constant
     * @returns {number} Initial balance value
     */
    function getInitialBalance() {
        return INITIAL_BALANCE;
    }

    return { execute, reset, getInitialBalance };
})();

// ============================================================================
// OPERATIONS MODULE (Equivalent to operations.cob - Operations)
// ============================================================================

/**
 * Operations - Business logic layer
 * Handles TOTAL, CREDIT, and DEBIT operations
 */
const Operations = (() => {
    /**
     * Format balance for display (matches COBOL PIC 9(6)V99 format)
     * @param {number} balance - Balance to format
     * @returns {string} Formatted balance string
     */
    function formatBalance(balance) {
        return balance.toFixed(2).padStart(9, '0');
    }

    /**
     * Get current balance (for testing)
     * @returns {number} Current balance
     */
    function getBalance() {
        return DataProgram.execute('READ');
    }

    /**
     * View current balance (TOTAL operation)
     * @returns {number} Current balance
     */
    function viewBalance() {
        const balance = DataProgram.execute('READ');
        console.log(`Current balance: ${formatBalance(balance)}`);
        return balance;
    }

    /**
     * Credit account with specified amount (for testing)
     * @param {number} amount - Amount to credit
     * @returns {number} New balance after credit
     */
    function credit(amount) {
        // Read current balance
        let balance = DataProgram.execute('READ');
        
        // Add amount to balance
        balance = balance + amount;
        
        // Write new balance
        DataProgram.execute('WRITE', balance);
        
        console.log(`Amount credited. New balance: ${formatBalance(balance)}`);
        return balance;
    }

    /**
     * Debit account with specified amount (for testing)
     * @param {number} amount - Amount to debit
     * @returns {{success: boolean, balance: number, message: string}} Result of debit operation
     */
    function debit(amount) {
        // Read current balance
        let balance = DataProgram.execute('READ');
        
        // Check for sufficient funds (Business Rule: no negative balances)
        if (balance >= amount) {
            // Subtract amount from balance
            balance = balance - amount;
            
            // Write new balance
            DataProgram.execute('WRITE', balance);
            
            console.log(`Amount debited. New balance: ${formatBalance(balance)}`);
            return { success: true, balance, message: `Amount debited. New balance: ${formatBalance(balance)}` };
        } else {
            console.log('Insufficient funds for this debit.');
            return { success: false, balance, message: 'Insufficient funds for this debit.' };
        }
    }

    /**
     * Credit account (CREDIT operation) - Interactive version
     * Adds specified amount to the account balance
     */
    function creditAccount() {
        const amountStr = readlineSync.question('Enter credit amount: ');
        const amount = parseFloat(amountStr) || 0;
        credit(amount);
    }

    /**
     * Debit account (DEBIT operation) - Interactive version
     * Subtracts specified amount from the account balance
     * Rejects transaction if insufficient funds
     */
    function debitAccount() {
        const amountStr = readlineSync.question('Enter debit amount: ');
        const amount = parseFloat(amountStr) || 0;
        debit(amount);
    }

    /**
     * Execute operation based on type
     * @param {string} operationType - 'TOTAL', 'CREDIT', or 'DEBIT'
     */
    function execute(operationType) {
        switch (operationType.trim()) {
            case 'TOTAL':
                viewBalance();
                break;
            case 'CREDIT':
                creditAccount();
                break;
            case 'DEBIT':
                debitAccount();
                break;
            default:
                console.log('Unknown operation.');
        }
    }

    return { execute, viewBalance, creditAccount, debitAccount, credit, debit, getBalance, formatBalance };
})();

// ============================================================================
// MAIN PROGRAM (Equivalent to main.cob - MainProgram)
// ============================================================================

/**
 * MainProgram - Entry point and menu system
 * Provides menu-driven interface for account management
 */
function MainProgram() {
    // WORKING-STORAGE SECTION
    // 01 CONTINUE-FLAG PIC X(3) VALUE 'YES'
    let continueFlag = 'YES';

    // MAIN-LOGIC: PERFORM UNTIL CONTINUE-FLAG = 'NO'
    while (continueFlag !== 'NO') {
        // Display menu
        console.log('--------------------------------');
        console.log('Account Management System');
        console.log('1. View Balance');
        console.log('2. Credit Account');
        console.log('3. Debit Account');
        console.log('4. Exit');
        console.log('--------------------------------');
        
        // Accept user choice
        const userChoice = readlineSync.question('Enter your choice (1-4): ');
        
        // EVALUATE USER-CHOICE
        switch (userChoice) {
            case '1':
                // CALL 'Operations' USING 'TOTAL'
                Operations.execute('TOTAL');
                break;
            case '2':
                // CALL 'Operations' USING 'CREDIT'
                Operations.execute('CREDIT');
                break;
            case '3':
                // CALL 'Operations' USING 'DEBIT'
                Operations.execute('DEBIT');
                break;
            case '4':
                // MOVE 'NO' TO CONTINUE-FLAG
                continueFlag = 'NO';
                break;
            default:
                // WHEN OTHER
                console.log('Invalid choice, please select 1-4.');
        }
    }

    // Exit message
    console.log('Exiting the program. Goodbye!');
}

// ============================================================================
// APPLICATION ENTRY POINT
// ============================================================================

// Run the main program only if this is the main module
if (isMainModule) {
    MainProgram();
}

// Export modules for testing
module.exports = {
    DataProgram,
    Operations,
    MainProgram
};
