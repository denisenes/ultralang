#include <stdint.h>
#include <stdio.h>

#include <cassert>
#include <list>
#include <vector>
#include <string>
#include <cstring>
#include <iostream>
#include <fstream>
#include <bitset>
#include <filesystem>
#include <functional>
#include <map>

#include "isa.h"

#define LOGGING_ENABLED

#define ERROR_EXECUTION 0
#define ERROR_RED_ZONE  1

#define MEM_DUMP_WIDTH 64

#define MEM_SIZE 64 * 1024
#define OPERAND_STACK_SIZE 64

#define DEFAULT_LSB_ON_START (MEM_SIZE - 2 * 1024)
#define DEFAULT_OSB_ON_START (MEM_SIZE - 4 * 1024)

#define RED_ZONE_START ((U16) 0)
#define RED_ZONE_END   ((U16) 8)

#define INTERRUPT_TABLE_SIZE 256

#ifdef LOGGING_ENABLED
#define LOG(format, ...) printf(format, __VA_ARGS__)
#else
#define LOG(format, ...) ((void*)0)
#endif

#define bytes_to_u16(b1, b2) ((b1 << UINT8_WIDTH) | b2)
#define u16_lo(value)        ((U8)(value & 0xFF))
#define u16_hi(value)        ((U8)(value >> 8))

#define WORD_SIZE 2
#define HALF_WORD_SIZE 1

#define terminate_if(condition, code, msg, ...) { \
    if (condition) {                              \
        printf(msg, __VA_ARGS__);                 \
        std::exit(code);                          \
    }                                             \
}

#define red_zone_check(addr)                            \
    terminate_if(                                       \
        RED_ZONE_START <= addr && addr < RED_ZONE_END,  \
        ERROR_RED_ZONE,                                 \
        "Red zone access fault [addr = %u]\n", addr);

using U8  = uint8_t;
using U16 = uint16_t;
using I8  = int8_t;
using I16 = int16_t;

enum AccessWidth {
    W8  = 0x1,
    W16 = 0x2
};

class Register {
    public:
        Register() = delete;

        Register(int from) {
            assert(0 <= from && from <= UINT16_MAX);
            val = from;
        }

        Register(U16 from) {
            val = from;
        }

        U16 get() {
            return static_cast<U16>(val);
        }

        void set(U16 v) {
            assert(0 <= v && v <= UINT16_MAX);
            val = v;
        }

        void advance(U16 diff) {
            val += diff; 
        }

        std::string asStrVerbose() {
            std::stringstream s;
            s << std::setfill(' ');
            s << "[unsigned: "  << std::setw(4) << val;
            s << ", signed: "   << std::setw(4) << static_cast<int16_t>(val);
            s << std::setfill('0');
            s << ", hex: "       << std::setw(4) << std::hex << val;
            s << ", bin: "   << std::setw(4) << std::bitset<16>(val);
            s << "]";
            return s.str();
        }

    private:
        U16 val;
};

class State {
    public:
        State() = delete;

        State(std::ifstream& image, int imageSize): 
            memory(MEM_SIZE, HALT), 
            ip(RED_ZONE_END),
            lsp(DEFAULT_LSB_ON_START), lsb(DEFAULT_LSB_ON_START), lfp(DEFAULT_LSB_ON_START),
            osp(DEFAULT_OSB_ON_START), osb(DEFAULT_OSB_ON_START)    
        {
            image.read(reinterpret_cast<char*>(memory.data()), imageSize);
        }

        U16 ipConsume8() {
            U8 byte = memory[ip.get()];
            ip.advance(1);
            return byte;
        }

        U16 ipConsume16() {
            U16 byte1 = memory[ip.get()];
            U16 byte2 = memory[ip.get() + 1];
            ip.advance(2);
            return bytes_to_u16(byte1, byte2);
        }

        void push(U16 value) {
            if (operandStack.size() >= OPERAND_STACK_SIZE) {
                // evacuate bottom value to memory
            }

            operandStack.push_back(Register(value));
        }

        U16 pop() {
            assert(operandStack.size() > 0);

            Register res = operandStack.back();
            operandStack.pop_back();

            if (osp.get() > osb.get()) {
                // load bottom value from memory
            }

            return res.get();
        }

        void dump(std::ostream& stream, bool isVerbose) {
            stream << "IP:  " << ip.asStrVerbose()  << "\n";
            stream << "LSP: " << lsp.asStrVerbose() << "\n";
            stream << "OSP: " << osp.asStrVerbose() << "\n";
            stream << "Operand stack:\n";
            for (auto& operand : operandStack) {
                stream << "\t" << operand.asStrVerbose() << "\n"; 
            }

            if (!isVerbose) {
                return;
            }

            stream << "Memory:\n";

            int i = 0;
            for (auto byte: memory) {
                stream << std::hex << std::setfill('0') << std::setw(2);
                stream << static_cast<U16>(byte) << " ";
                i++;

                if (i == MEM_DUMP_WIDTH) {
                    stream << "\n";
                    i = 0;
                }
            }
        }

        U16 loadFromMemory(AccessWidth width, U16 address) {
            red_zone_check(address);

            switch (width) {
                case W8:  return memory[address];
                case W16: return bytes_to_u16(memory[address], memory[address+1]);
                default:
                    terminate_if(true, ERROR_EXECUTION, "Unknown width: %u\n", width);
                    return 0;
            }
        }

        void storeToMemory(AccessWidth width, U16 address, U16 value) {
            red_zone_check(address);

            switch (width) {
                case W8: 
                    memory[address] = u16_lo(value);
                    break;
                case W16:
                    memory[address]   = u16_hi(value);
                    memory[address+1] = u16_lo(value);
                    break;
            }
        }

        void jumpTo(U16 addr) {
            red_zone_check(addr);
            ip.set(addr);
        }

        U16 getIP() {
            return ip.get();
        }

        void saveRetAddr() {
            retAddrStack.push_back(ip.get());
        }

        void prepareBeforeCall(int argsCount) {
            saveRetAddr();

            pushLocal(lfp.get());
            lfp.set(lsp.get());
            
            for (int i = 0; i < argsCount; i++) {
                pushLocal(pop());
            }
        }

        void returnToCaller() {
            restoreRetAddr();

            U16 callerFrameEnd = lfp.get() - WORD_SIZE;

            lsp.set(callerFrameEnd);
            lfp.set(memory[callerFrameEnd]);
        }

    private:
        /* Current memory layout:
         * +----------------+-------------------+------------+-----------+
         * | Red zone (8 B) | application space | BOS (2 KB) | LS (2 KB) |
         * +----------------+-------------------+------------+-----------+
         * 
         * Red zone - memory that cannot be accessed or executed.
         * BOS - backup operand stack: used to store least recently used values from operand stack when it is overfilled.
         * LS  - locals stack: contains local variables of functions.
         * 
         * Image is placed right after red zone.
         */

        Register ip;  // instruction pointer
        Register lsb; // locals stack base
        Register lfp; // locals frame pointer
        Register lsp; // locals stack pointer
        Register osb; // operand stack base
        Register osp; // operand stack pointer
        std::vector<U8> memory;
        std::list<Register> operandStack;
        std::list<U16> retAddrStack;

        void pushLocal(U16 value) {
            memory[lsp.get()] = value;
            lsp.advance(2);
        }

        void restoreRetAddr() {
            assert(!retAddrStack.empty());

            U16 retAddr = retAddrStack.back();
            retAddrStack.pop_back();

            red_zone_check(retAddr);
            ip.set(retAddr);
        }
};

class InterruptController {
    public:
        InterruptController(): interruptRoutineTable(INTERRUPT_TABLE_SIZE) {};

        void setInterruptHandler(U8 code, U16 address) {
            red_zone_check(address);
            interruptRoutineTable[code] = address;
        }

        void saveContextAndDoInterrupt(State& state, U8 interruptCode) {
            state.saveRetAddr();
            state.jumpTo(interruptRoutineTable[interruptCode]);
        }

        void syncInterrupt(State& state, U8 interruptCode) {
            saveContextAndDoInterrupt(state, interruptCode);
        }

        void asyncInterrupt(State& state) {
            while(outerRequests.any()) {
                int outerID = -1;
                for (int i = 0; i < INTERRUPT_TABLE_SIZE; i++) {
                    if (outerRequests[i]) {
                        outerID = i;
                        outerRequests[i] = false; 
                        break;
                    }
                }
                assert(outerID != -1);

                U16 interruptCode = redirectionMap[(U16) outerID];
                saveContextAndDoInterrupt(state, interruptCode);
            }
        }

    private:
        std::vector<U16> interruptRoutineTable;
        std::bitset<INTERRUPT_TABLE_SIZE> outerRequests; // TODO use
        std::map<U16, U16> redirectionMap;               // TODO fill
};

void unaryOp(State& state, std::function<U16(U16)> op) {
    U16 v = state.pop();
    state.push(op(v));
}

void binOp(State& state, std::function<U16(U16, U16)> op) {
    U16 v1 = state.pop();
    U16 v2 = state.pop();
    state.push(op(v1, v2));
}

void interpretationLoop(State& state, InterruptController intController) {
    while (true) {
        U16 opcode = state.ipConsume8();
        LOG("Opcode: 0x%x\n", opcode);

        // TODO debug mode

        switch (opcode) {
            case HALT:
                // stop execution
                return;
            case CONST_ZERO:
                state.push((U16) 0);
                break;
            case CONSTW: {
                U16 constVal = state.ipConsume16();
                state.push(constVal);
                break;
            }
            case CONST: {
                U16 constVal = state.ipConsume8();
                state.push(constVal);
                break;
            }
            case DUP: {
                U16 topValue = state.pop();
                state.push(topValue);
                state.push(topValue);
                break;
            }
            case SWAP: {
                U16 top = state.pop();
                U16 second = state.pop();
                state.push(top);
                state.push(second);
                break;
            }
            case DROP:
                state.pop();
                break;
            case BIN_ADD:
                binOp(state, [](U16 v1, U16 v2){ return v1 + v2; }); break;
            case BIN_SUB:
                binOp(state, [](U16 v1, U16 v2){ return v1 - v2; }); break;
            case BIN_MUL:
                binOp(state, [](U16 v1, U16 v2){ return v1 * v2; }); break;
            case BIN_DIV:
                binOp(state, [](U16 v1, U16 v2){ return v1 / v2; }); break;
            case BIN_OR:
                binOp(state, [](U16 v1, U16 v2){ return v1 | v2; }); break;
            case BIN_AND:
                binOp(state, [](U16 v1, U16 v2){ return v1 & v2; }); break;
            case BIN_XOR:
                binOp(state, [](U16 v1, U16 v2){ return v1 ^ v2; }); break;
            case BIN_SHR:
                binOp(state, [](U16 v1, U16 v2){ return v1 >> v2; }); break;
            case BIN_SHL:
                binOp(state, [](U16 v1, U16 v2){ return v1 << v2; }); break;
            case BIN_CMP_U:
                binOp(state, [](U16 v1, U16 v2) {
                    return v1 > v2 ?  1 :
                           v1 < v2 ? -1 : 0;
                });
                break;
            case BIN_CMP_S:
                binOp(state, [](U16 v1, U16 v2) {
                    I16 sv1 = static_cast<I16>(v1);
                    I16 sv2 = static_cast<I16>(v2); 
                    return sv1 > sv2 ?  1 :
                           sv1 < sv2 ? -1 : 0; 
                });
                break;
            case UN_NEG:
                unaryOp(state, [](U16 v){ return -v; });
                break;
            case UN_INC:
                unaryOp(state, [](U16 v){ return v++; });
                break;
            case UN_DEC:
                unaryOp(state, [](U16 v){ return v--; });
                break;
            case UN_CMP_ZERO_U:
                unaryOp(state, [](U16 v) {
                    return v > ((U16) 0) ?  1 : 0;
                });
                break;
            case UN_CMP_ZERO_S:
                unaryOp(state, [](U16 v) {
                    I16 sv = static_cast<I16>(v);
                    return sv > ((I16) 0) ?  1 :
                           sv < ((I16) 0) ? -1 : 0;
                });
                break;
            case LOAD_IP:
                state.push(state.getIP() - 1);
                break;
            case LOAD:
            case LOAD_OFFS:
            case LOAD_DYN_OFFS: {
                AccessWidth width = static_cast<AccessWidth>(state.ipConsume8());
                U16 address = state.pop();
                
                U16 offset = 
                    (opcode == LOAD_OFFS)     ? state.ipConsume16() :
                    (opcode == LOAD_DYN_OFFS) ? state.pop()         :
                    0;

                U16 loaded = state.loadFromMemory(width, address + offset);
                state.push(loaded);
                break;
            }
            case STORE:
            case STORE_OFFS:
            case STORE_DYN_OFFS: {
                AccessWidth width = static_cast<AccessWidth>(state.ipConsume8());
                U16 address = state.pop();
                U16 value   = state.pop();

                U16 offset = 
                    (opcode == STORE_OFFS)     ? state.ipConsume16() :
                    (opcode == STORE_DYN_OFFS) ? state.pop()         :
                    0;

                state.storeToMemory(width, address, value);
                break;
            }
            case BR:
            case BR_DYN: {
                U16 address = (opcode == BR) ? state.ipConsume16() : state.pop();
                state.jumpTo(address);
                break;
            }
            case BR_IF:
            case BR_IF_DYN: {
                U16 expected = state.ipConsume8(); // TODO encode more efficiently
                U16 flag = state.pop();
                U16 address = (opcode == BR_IF) ? state.ipConsume16() : state.pop();

                if (flag == expected) {
                    state.jumpTo(address);
                }
                break;
            }
            case CALL:
            case CALL_DYN: {
                U16 address = (opcode == CALL) ? state.ipConsume16() : state.pop();
                U16 args = state.ipConsume8();

                state.prepareBeforeCall(args);
                state.jumpTo(address);
                break;
            }
            case RET:
                state.returnToCaller();
                break;
            case INT: {
                U16 code = state.ipConsume8();
                intController.syncInterrupt(state, code);
                break;
            }
            case INT_SET_HANDLER: {
                U8 code = state.ipConsume8();
                U16 address = state.ipConsume16();
                intController.setInterruptHandler(code, address);
                break;
            }
            default:
                terminate_if(true, ERROR_EXECUTION, "Unknown opcode: %u\n", opcode);
        }
    }
}

void interpret(char* imagePath, bool isVerbose) {
    size_t imageSize = std::filesystem::file_size(imagePath);
    std::ifstream image(imagePath, std::ios::binary);
    
    State state(image, imageSize);
    InterruptController intController;

    interpretationLoop(state, intController);

    state.dump(std::cout, isVerbose);
}

int main(int argc, char** argv) {
    bool isVerbose = false;
    char* imagePath; 

    switch (argc) {
        case 2:
            imagePath = argv[1];
            break;
        case 3:
            if (strcmp(argv[1], "-v") == 0) isVerbose = true;
            imagePath = argv[2];
            break;
        default:
            printf("Usage: ucorn-emu <path to .img>\n");
            return 1;
    }


    interpret(imagePath, isVerbose);
    
    return 0;
}