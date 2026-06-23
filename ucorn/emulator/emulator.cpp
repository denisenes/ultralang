#include <stdio.h>

#include <bitset>
#include <cassert>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <functional>
#include <iostream>
#include <list>
#include <map>
#include <string>
#include <vector>

#include "isa.h"
#include "types.h"

#define LOGGING_ENABLED

#define ERROR_EXECUTION 0
#define ERROR_RED_ZONE  1

#define MEM_DUMP_WIDTH 64

#define MEM_SIZE           64 * 1024
#define OPERAND_STACK_SIZE 64

#define DEFAULT_LSB_ON_START (MEM_SIZE - 2 * 1024)
#define DEFAULT_OSB_ON_START (MEM_SIZE - 4 * 1024)

#define RED_ZONE_START ((U16)0)
#define RED_ZONE_END   ((U16)8)

#define INTERRUPT_TABLE_SIZE 256

#ifdef LOGGING_ENABLED
#define LOG(format, ...) printf(format, __VA_ARGS__)
#else
#define LOG(format, ...) ((void*)0)
#endif

#define WORD_SIZE      2
#define HALF_WORD_SIZE 1

#define TERMINATE_IF(condition, code, msg, ...) \
    {                                           \
        if (condition) {                        \
            printf(msg, __VA_ARGS__);           \
            std::exit(code);                    \
        }                                       \
    }                                           \
    while (0)

#define RED_ZONE_CHECK(addr)                                    \
    TERMINATE_IF(RED_ZONE_START <= addr && addr < RED_ZONE_END, \
                 ERROR_RED_ZONE, "Red zone access fault [addr = %u]\n", addr);

enum AccessWidth { W8 = 0x1, W16 = 0x2 };

class Register {
public:
    Register() = delete;

    Register(int from)
    {
        assert(0 <= from && from <= UINT16_MAX);
        val = from;
    }

    Register(U16 from) { val = from; }

    U16 Get() { return static_cast<U16>(val); }

    void Set(U16 v)
    {
        assert(0 <= v && v <= UINT16_MAX);
        val = v;
    }

    void Advance(U16 diff) { val += diff; }

    std::string AsStrVerbose()
    {
        std::stringstream s;
        s << std::setfill(' ');
        s << "[unsigned: " << std::setw(4) << val;
        s << ", signed: " << std::setw(4) << static_cast<int16_t>(val);
        s << std::setfill('0');
        s << ", hex: " << std::setw(4) << std::hex << val;
        s << ", bin: " << std::setw(4) << std::bitset<16>(val);
        s << "]";
        return s.str();
    }

private:
    U16 val;
};

class State {
public:
    State() = delete;

    State(std::ifstream& image, int imageSize)
        : memory(MEM_SIZE, HALT),
          ip(RED_ZONE_END),
          lsp(DEFAULT_LSB_ON_START),
          lsb(DEFAULT_LSB_ON_START),
          lfp(DEFAULT_LSB_ON_START),
          osp(DEFAULT_OSB_ON_START),
          osb(DEFAULT_OSB_ON_START)
    {
        image.read(reinterpret_cast<char*>(memory.data()), imageSize);
    }

    U16 IpConsume8()
    {
        U8 byte = memory[ip.Get()];
        ip.Advance(1);
        return byte;
    }

    U16 IpConsume16()
    {
        U16 byte1 = memory[ip.Get()];
        U16 byte2 = memory[ip.Get() + 1];
        ip.Advance(2);
        return U16_FROM_BYTES(byte1, byte2);
    }

    void Push(U16 value)
    {
        if (operandStack.size() >= OPERAND_STACK_SIZE) {
            // evacuate bottom value to memory
        }

        operandStack.push_back(Register(value));
    }

    U16 Pop()
    {
        assert(operandStack.size() > 0);

        Register res = operandStack.back();
        operandStack.pop_back();

        if (osp.Get() > osb.Get()) {
            // load bottom value from memory
        }

        return res.Get();
    }

    void Dump(std::ostream& stream, bool isVerbose)
    {
        stream << "IP:  " << ip.AsStrVerbose() << "\n";
        stream << "LSP: " << lsp.AsStrVerbose() << "\n";
        stream << "OSP: " << osp.AsStrVerbose() << "\n";
        stream << "Operand stack:\n";
        for (auto& operand : operandStack) {
            stream << "\t" << operand.AsStrVerbose() << "\n";
        }

        if (!isVerbose) {
            return;
        }

        stream << "Memory:\n";

        int i = 0;
        for (auto byte : memory) {
            stream << std::hex << std::setfill('0') << std::setw(2);
            stream << static_cast<U16>(byte) << " ";
            i++;

            if (i == MEM_DUMP_WIDTH) {
                stream << "\n";
                i = 0;
            }
        }
    }

    U16 LoadFromMemory(AccessWidth width, U16 address)
    {
        RED_ZONE_CHECK(address);

        switch (width) {
            case W8:
                return memory[address];
            case W16:
                return U16_FROM_BYTES(memory[address], memory[address + 1]);
            default:
                TERMINATE_IF(true, ERROR_EXECUTION, "Unknown width: %u\n",
                             width);
                return 0;
        }
    }

    void StoreToMemory(AccessWidth width, U16 address, U16 value)
    {
        RED_ZONE_CHECK(address);

        switch (width) {
            case W8:
                memory[address] = U16_LO(value);
                break;
            case W16:
                memory[address]     = U16_HI(value);
                memory[address + 1] = U16_LO(value);
                break;
        }
    }

    void JumpTo(U16 addr)
    {
        RED_ZONE_CHECK(addr);
        ip.Set(addr);
    }

    U16 GetIp() { return ip.Get(); }

    void SaveRetAddr() { retAddrStack.push_back(ip.Get()); }

    void PrepareBeforeCall(int argsCount)
    {
        SaveRetAddr();

        PushLocal(lfp.Get());
        lfp.Set(lsp.Get());

        for (int i = 0; i < argsCount; i++) {
            PushLocal(Pop());
        }
    }

    void ReturnToCaller()
    {
        RestoreRetAddr();

        U16 callerFrameEnd = lfp.Get() - WORD_SIZE;

        lsp.Set(callerFrameEnd);
        lfp.Set(memory[callerFrameEnd]);
    }

private:
    /* Current memory layout:
     * +----------------+-------------------+------------+-----------+
     * | Red zone (8 B) | application space | BOS (2 KB) | LS (2 KB) |
     * +----------------+-------------------+------------+-----------+
     *
     * Red zone - memory that cannot be accessed or executed.
     * BOS - backup operand stack: used to store least recently used values from
     * operand stack when it is overfilled. LS  - locals stack: contains local
     * variables of functions.
     *
     * Image is placed right after red zone.
     */

    Register            ip;   // instruction pointer
    Register            lsb;  // locals stack base
    Register            lfp;  // locals frame pointer
    Register            lsp;  // locals stack pointer
    Register            osb;  // operand stack base
    Register            osp;  // operand stack pointer
    std::vector<U8>     memory;
    std::list<Register> operandStack;
    std::list<U16>      retAddrStack;

    void PushLocal(U16 value)
    {
        memory[lsp.Get()] = value;
        lsp.Advance(2);
    }

    void RestoreRetAddr()
    {
        assert(!retAddrStack.empty());

        U16 retAddr = retAddrStack.back();
        retAddrStack.pop_back();

        RED_ZONE_CHECK(retAddr);
        ip.Set(retAddr);
    }
};

class InterruptController {
public:
    InterruptController() : interruptRoutineTable(INTERRUPT_TABLE_SIZE) {};

    void SetInterruptHandler(U8 code, U16 address)
    {
        RED_ZONE_CHECK(address);
        interruptRoutineTable[code] = address;
    }

    void SaveContextAndDoInterrupt(State& state, U8 interruptCode)
    {
        state.SaveRetAddr();
        state.JumpTo(interruptRoutineTable[interruptCode]);
    }

    void SyncInterrupt(State& state, U8 interruptCode)
    {
        SaveContextAndDoInterrupt(state, interruptCode);
    }

    void AsyncInterrupt(State& state)
    {
        while (outerRequests.any()) {
            int outerID = -1;
            for (int i = 0; i < INTERRUPT_TABLE_SIZE; i++) {
                if (outerRequests[i]) {
                    outerID          = i;
                    outerRequests[i] = false;
                    break;
                }
            }
            assert(outerID != -1);

            U16 interruptCode = redirectionMap[(U16)outerID];
            SaveContextAndDoInterrupt(state, interruptCode);
        }
    }

private:
    std::vector<U16>                  interruptRoutineTable;
    std::bitset<INTERRUPT_TABLE_SIZE> outerRequests;   // TODO use
    std::map<U16, U16>                redirectionMap;  // TODO fill
};

void UnaryOp(State& state, std::function<U16(U16)> op)
{
    U16 v = state.Pop();
    state.Push(op(v));
}

void BinOp(State& state, std::function<U16(U16, U16)> op)
{
    U16 v1 = state.Pop();
    U16 v2 = state.Pop();
    state.Push(op(v1, v2));
}

void InterpretationLoop(State& state, InterruptController intController)
{
    while (true) {
        U16 opcode = state.IpConsume8();
        LOG("Opcode: 0x%x\n", opcode);

        // TODO debug mode

        switch (opcode) {
            case HALT:
                // stop execution
                return;
            case CONST_ZERO:
                state.Push((U16)0);
                break;
            case CONSTW: {
                U16 constVal = state.IpConsume16();
                state.Push(constVal);
                break;
            }
            case CONST: {
                U16 constVal = state.IpConsume8();
                state.Push(constVal);
                break;
            }
            case DUP: {
                U16 topValue = state.Pop();
                state.Push(topValue);
                state.Push(topValue);
                break;
            }
            case SWAP: {
                U16 top    = state.Pop();
                U16 second = state.Pop();
                state.Push(top);
                state.Push(second);
                break;
            }
            case DROP:
                state.Pop();
                break;
            case BIN_ADD:
                BinOp(state, [](U16 v1, U16 v2) { return v1 + v2; });
                break;
            case BIN_SUB:
                BinOp(state, [](U16 v1, U16 v2) { return v1 - v2; });
                break;
            case BIN_MUL:
                BinOp(state, [](U16 v1, U16 v2) { return v1 * v2; });
                break;
            case BIN_DIV:
                BinOp(state, [](U16 v1, U16 v2) { return v1 / v2; });
                break;
            case BIN_OR:
                BinOp(state, [](U16 v1, U16 v2) { return v1 | v2; });
                break;
            case BIN_AND:
                BinOp(state, [](U16 v1, U16 v2) { return v1 & v2; });
                break;
            case BIN_XOR:
                BinOp(state, [](U16 v1, U16 v2) { return v1 ^ v2; });
                break;
            case BIN_SHR:
                BinOp(state, [](U16 v1, U16 v2) { return v1 >> v2; });
                break;
            case BIN_SHL:
                BinOp(state, [](U16 v1, U16 v2) { return v1 << v2; });
                break;
            case BIN_CMP_U:
                BinOp(state, [](U16 v1, U16 v2) {
                    return v1 > v2 ? 1 : v1 < v2 ? -1 : 0;
                });
                break;
            case BIN_CMP_S:
                BinOp(state, [](U16 v1, U16 v2) {
                    I16 sv1 = static_cast<I16>(v1);
                    I16 sv2 = static_cast<I16>(v2);
                    return sv1 > sv2 ? 1 : sv1 < sv2 ? -1 : 0;
                });
                break;
            case UN_NEG:
                UnaryOp(state, [](U16 v) { return -v; });
                break;
            case UN_INC:
                UnaryOp(state, [](U16 v) { return v++; });
                break;
            case UN_DEC:
                UnaryOp(state, [](U16 v) { return v--; });
                break;
            case UN_CMP_ZERO_U:
                UnaryOp(state, [](U16 v) { return v > ((U16)0) ? 1 : 0; });
                break;
            case UN_CMP_ZERO_S:
                UnaryOp(state, [](U16 v) {
                    I16 sv = static_cast<I16>(v);
                    return sv > ((I16)0) ? 1 : sv < ((I16)0) ? -1 : 0;
                });
                break;
            case LOAD_IP:
                state.Push(state.GetIp() - 1);
                break;
            case LOAD:
            case LOAD_OFFS:
            case LOAD_DYN_OFFS: {
                AccessWidth width =
                    static_cast<AccessWidth>(state.IpConsume8());
                U16 address = state.Pop();

                U16 offset = (opcode == LOAD_OFFS)       ? state.IpConsume16()
                             : (opcode == LOAD_DYN_OFFS) ? state.Pop()
                                                         : 0;

                U16 loaded = state.LoadFromMemory(width, address + offset);
                state.Push(loaded);
                break;
            }
            case STORE:
            case STORE_OFFS:
            case STORE_DYN_OFFS: {
                AccessWidth width =
                    static_cast<AccessWidth>(state.IpConsume8());
                U16 address = state.Pop();
                U16 value   = state.Pop();

                U16 offset = (opcode == STORE_OFFS)       ? state.IpConsume16()
                             : (opcode == STORE_DYN_OFFS) ? state.Pop()
                                                          : 0;

                state.StoreToMemory(width, address, value);
                break;
            }
            case BR:
            case BR_DYN: {
                U16 address =
                    (opcode == BR) ? state.IpConsume16() : state.Pop();
                state.JumpTo(address);
                break;
            }
            case BR_IF:
            case BR_IF_DYN: {
                U16 expected =
                    state.IpConsume8();  // TODO encode more efficiently
                U16 flag = state.Pop();
                U16 address =
                    (opcode == BR_IF) ? state.IpConsume16() : state.Pop();

                if (flag == expected) {
                    state.JumpTo(address);
                }
                break;
            }
            case CALL:
            case CALL_DYN: {
                U16 address =
                    (opcode == CALL) ? state.IpConsume16() : state.Pop();
                U16 args = state.IpConsume8();

                state.PrepareBeforeCall(args);
                state.JumpTo(address);
                break;
            }
            case RET:
                state.ReturnToCaller();
                break;
            case INT: {
                U16 code = state.IpConsume8();
                intController.SyncInterrupt(state, code);
                break;
            }
            case INT_SET_HANDLER: {
                U8  code    = state.IpConsume8();
                U16 address = state.IpConsume16();
                intController.SetInterruptHandler(code, address);
                break;
            }
            default:
                TERMINATE_IF(true, ERROR_EXECUTION, "Unknown opcode: %u\n",
                             opcode);
        }
    }
}

void Interpret(char* imagePath, bool isVerbose)
{
    size_t        imageSize = std::filesystem::file_size(imagePath);
    std::ifstream image(imagePath, std::ios::binary);

    State               state(image, imageSize);
    InterruptController intController;

    InterpretationLoop(state, intController);

    state.Dump(std::cout, isVerbose);
}

int main(int argc, char** argv)
{
    bool  isVerbose = false;
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

    Interpret(imagePath, isVerbose);

    return 0;
}