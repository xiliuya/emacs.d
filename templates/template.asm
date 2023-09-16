        ;; filename     : `(buffer-name)`
        ;; created at   : `(format-time-string "%F %T")`
        ;; author       : `user-full-name` <`user-mail-address`>
        ;; version      : 1.0
        ;; description  : 一个 Linux 下汇编应用程序, 使用 NASM 2.16
        ;; .
        ;; compile command:
        ;; nasm -f elf -g -F stabs `(buffer-name)`
        ;; ld -m elf_i386 -o `(file-name-base (buffer-name))` `(file-name-base (buffer-name))`.o
        ;;

        SECTION .data           ; 包含已初始化的数据的段

        SECTION .bss            ; 包含未初始化的数据的段
        SECTION .text           ; 包含代码的段

        global _start           ;连接器需要据此找到入口点

_start:
        nop                     ; 这个无操作指令让 gdb 很高兴

        jmp _exit

_exit:
        mov eax,1               ; 指定 Exit 系统调用
        mov ebx,0               ; 返回一个零值
        int 80H                 ; 进行系统调用来终止程序

        ;; Local Variables:
        ;; mode: asm
        ;; fill-column: 70
        ;; comment-column: 30
        ;; compile-command: "nasm -f elf -g -F stabs `(buffer-name)` && ld -m elf_i386 -o `(file-name-base (buffer-name))` `(file-name-base (buffer-name))`.o"
        ;; End:
