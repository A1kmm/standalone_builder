#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#define SYSCALL "push %%ebp;" \
                "mov %%ecx, %%ebp;" \
                "syscall;" \
                "mov $0x2b,%%ecx;" \
                "mov %%ecx,%%ss;" \
                "mov %%ebp,%%ecx;" \
                "pop %%ebp;"

int open(const char* pathname, int flags, int mode)
{
  int result;
  asm("movl $0x5, %%eax;" SYSCALL
      : "=a"(result) : "b"(pathname), "c"(flags), "d"(mode));
  return result;
}

void exit(int status)
{
  asm("movl $0x1, %%eax;" SYSCALL : : "b"(status));
  exit(status); // gcc error suppression.
}

int execve(const char* filename, char* const argv[], char* const envp[])
{
  int result;
  asm("movl $11, %%eax;" SYSCALL : "=a"(result)
      : "b"(filename), "c"(argv), "d"(envp));
  return result;
}

pid_t waitpid(pid_t pid, int* status, int options)
{
  pid_t result;
  asm("movl $7, %%eax;" SYSCALL : "=a"(result)
      : "b"(pid), "c"(status), "d"(options));
  return result;
}

int mkdir(const char* filename, mode_t mode)
{
  int result;
  asm("movl $39, %%eax;" SYSCALL : "=a"(result)
      : "b"(filename), "c"(mode));
  return result;
}

int pipe(int pipefd[2])
{
  int result;
  asm("movl $42, %%eax;" SYSCALL
      : "=a"(result) : "b"(pipefd));
  return result;
}

pid_t fork(void)
{
  pid_t result;
  asm("movl $2, %%eax;" SYSCALL
      : "=a"(result));
  return result;
}

int dup2(int oldfd, int newfd)
{
  int result;
  asm("movl $63, %%eax;" SYSCALL
      : "=a"(result) : "b"(oldfd), "c"(newfd));
  return result;
}

int write(int fd, const void* buf, size_t count)
{
  int result;
  asm("movl $0x4, %%eax;" SYSCALL
      : "=a"(result) : "b"(fd), "c"(buf), "d"(count));
  return result;
}

int read(int fd, void* buf, size_t count)
{
  int result;
  asm("movl $0x3, %%eax;" SYSCALL
      : "=a"(result) : "b"(fd), "c"(buf), "d"(count));
  return result;
}

int ftruncate(int fd, off_t length)
{
  int result;
  asm("movl $93, %%eax;" SYSCALL
      : "=a"(result) : "b"(fd), "c"(length));
  return result;
}

#ifdef USE_BRK
void* brk(void* addr)
{
  void* result;
  asm("movl $45, %%eax;" SYSCALL
      : "=a"(result) : "b"(addr));
  return result;
}
#endif

void* mmap(void *addr, size_t length, int prot, int flags,
           int fd, off_t offset)
{
  void* result;
  offset >>= 13;
  asm("push %%ebp;"
      "movl %6, %%ebp;"
      "movl $192, %%eax;" SYSCALL
      "pop %%ebp;"
      : "=a"(result) : "b"(addr), "c"(length), "d"(prot), "S"(flags), "D"(fd),
                       "m"(offset));
  return result;
}

int munmap(void* addr, size_t length)
{
  int result;
  asm("movl $0x5b, %%eax;" SYSCALL
      : "=a"(result) : "b"(addr), "c"(length));
  return result;
}

int chdir(const char* path)
{
  int result;
  asm("movl $12, %%eax;" SYSCALL
      : "=a"(result) : "b"(path));
  return result;
}

int symlink(const char* oldpath, const char* newpath)
{
  int result;
  asm("movl $83, %%eax;" SYSCALL
      : "=a"(result) : "b"(oldpath), "c"(newpath));
  return result;  
}

int close(int fd)
{
  int result;
  asm("movl $6, %%eax;" SYSCALL
      : "=a"(result) : "b"(fd));
  return result;
}

size_t strlen(const char* str)
{
  size_t l = 0;
  while (*str++ != 0) l++;
  return l;
}

#ifdef USE_BRK
static char* dynend = NULL;
#endif
void* malloc(size_t sz)
{
#ifdef USE_BRK
  if (dynend == NULL)
    dynend = &end;
  void* addr = dynend;
  if (sz <= 0)
    return NULL;
  dynend += sz;
  if (brk(dynend) == addr)
  {
    return NULL;
  }
  return addr;
#else
  if (((sz - 4) & 0xFFF) != 0)
  {
    sz = ((sz - 4) | 0xFFF) + 1 - 4;
  }
  void** r = mmap(NULL, sz + 4, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  *r = (void*)sz;
  return (void*)(r + 1);
#endif
}

void free(void* ptr)
{
#ifdef USE_BRK
  /* No operation */
#else
  void** pptr = ptr;
  munmap(pptr-1, (size_t)pptr[-1]);
#endif
}

void* memcpy(void* dest, const void* src, size_t n)
{
  if (dest > src)
  {
    // Copy backwards...
    char* dptr = ((char*)dest) + n - 1;
    const char* sptr = ((char*)src) + n - 1;
    while (n)
    {
      n--;
      *dptr-- = *sptr--;
    }
  }
  else
  {
    // Copy forwards...
    char* dptr = dest;
    const char* sptr = src;
    while (n)
    {
      n--;
      *dptr++ = *sptr++;
    }
  }

  return dest;
}

void* memmove(void* dest, const void* src, size_t n)
{
  return memcpy(dest, src, n);
}

void* memset(void* s, int c, size_t n)
{
  char * pc = s;

  unsigned long mask4 = (c | c << 8 | c << 16 | c << 24);
  while (n && (((unsigned long)pc) & 0x3) != 0)
  {
    *pc++ = c;
    n--;
  }

  unsigned long* p = (unsigned long*)pc;
  while (n >= 4)
  {
    *p++ = mask4;
    n -= 4;
  }

  pc = (char*)p;
  while (n)
  {
    *pc++ = c;
    n--;
  }

  return s;
}

int
memcmp(const void* s1, const void* s2, size_t n)
{
  const unsigned char* pc1 = s1, * pc2 = s2;
  while (n)
  {
    if (*pc1 != *pc2)
      return *pc1 - *pc2;
    pc1++;
    pc2++;
    n--;
  }

  return 0;
}

