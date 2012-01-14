#include "xz.h"
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/wait.h>

void exit(int status);
int mkdir(const char* filename, mode_t mode);

#ifndef PRODUCT
#define PRODUCT "ProdTimer"
#endif
#ifndef PRODUCT_INSTALL_SUFFIX
#define PRODUCT_INSTALL_SUFFIX PRODUCT
#endif

#define CONSTOUT(v) write(1, v, sizeof(v) - 1)

enum tarstate {
  IN_HEADER,
  IN_FILENAME,
  IN_FILE
};

struct fileheader {
  size_t namelen;
  size_t filelen;
  unsigned int mode;
};

struct simpletar
{
  enum tarstate state;
  size_t offset;
  size_t len;
  char* ptr, *ptrorig;
  struct fileheader header;
  char filename[512];
};

/* Note: the start of the strings must not appear later in the string. */
static char* substReplace[] = {
  "SUBSTSA78901234567890123456789012345678901234567",
  "SUBSTSB7890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123"
};
static char* substSearch[] = {
  "STRINGA78901234567890123456789012345678901234567",
  "STRINGB7890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123"
};
static int substLength[] = {48, 344};
#define SubstCount 2

void
process_data(struct xz_buf* b, struct simpletar* st)
{
  int done = 0;
  while (done != b->out_pos)
  {
    size_t n = b->out_pos - done;
    if (n > st->len)
      n = st->len;
    memcpy(st->ptr, b->out, n);
    done += n;
    st->ptr += n;
    st->len -= n;
    int fd, i;
    int hit[SubstCount];
    for (i = 0; i < SubstCount; i++)
      hit[i] = 0;

    if (st->len == 0)
    {
      switch (st->state)
      {
      case IN_HEADER:
        if (st->header.namelen > 511)
          st->header.namelen = 511;
        st->len = st->header.namelen;
        st->ptr = st->filename;
        st->state = IN_FILENAME;
        continue;
      case IN_FILENAME:
        st->state = IN_FILE;
        st->len = st->header.filelen;
        st->filename[st->header.filelen] = 0;
        fd = open(st->filename, O_CREAT | O_RDWR, (mode_t)st->header.mode);
        st->ptrorig = st->ptr =
          mmap(NULL, st->header.filelen, PROT_READ | PROT_WRITE,
               0, fd, 0);
        close(fd);
        continue;
      case IN_FILE:
        st->ptr = st->ptrorig;
        {
          for (fd = 0; fd < st->header.filelen; fd++)
          {
            for (i = 0; i < SubstCount; i++)
            {
              if (*st->ptr == substSearch[i][hit[i]])
                hit[i]++;
              else
                hit[i] = 0;
              if (hit[i] == substLength[i])
              {
                memcpy(st->ptr - 1 - substLength[i], substReplace, substLength[i]);
                for (i = 0; i < SubstCount; i++)
                  hit[i] = 0;
              }
            }
            st->ptr++;
          }
        }
        munmap(st->ptrorig, st->header.filelen);
        continue;
      }
    }
  }
}

char **envp;

void message(const char* msg)
{
  pid_t p = fork();
  int status;
  if (p == 0)
  {
    const char* argv_gd[] = {"/usr/bin/gdialog", "--title", "Install",
                             "--msgbox", msg, "10", "60", NULL};
    execve("/usr/bin/gdialog", (char**)argv_gd, envp);
    execve("/usr/bin/kdialog", (char**)argv_gd, envp);
    execve("/usr/bin/whiptail", (char**)argv_gd, envp);
    execve("/usr/bin/dialog", (char**)argv_gd, envp);
    write(1, msg, strlen(msg));
    exit(0);
  }
  else
    waitpid(p, &status, 0);
}

const char* question(const char* msg, const char* defaultVal,
                     char* buf, size_t* bufPos)
{
  int status;
  int pipefd[2];
  int bufLen = *bufPos;
  *bufPos = 0;

  pipe(pipefd);

  pid_t p = fork();
  if (p == 0)
  {
    dup2(pipefd[1], 2);
    const char* argv_gd[] = {"/usr/bin/gdialog", "--title", "Install",
                             "--inputbox", msg, "10", "60", defaultVal, NULL};
    const char* argv_kd[] = {"/usr/bin/kdialog", "--title", "Install",
                             "--inputbox", msg, defaultVal, NULL};
    execve("/usr/bin/gdialog", (char**)argv_gd, envp);
    /* 'Back up' stdout in stderr, then set stdout to the pipe... */
    dup2(1, 2);
    dup2(pipefd[1], 1);
    execve("/usr/bin/kdialog", (char**)argv_kd, envp);
    /* Restore stdout from stderr, and put stderr back to being the pipe... */
    dup2(2, 1);
    dup2(pipefd[1], 2);
    execve("/usr/bin/whiptail", (char**)argv_gd, envp);
    execve("/usr/bin/dialog", (char**)argv_gd, envp);
    write(1, msg, strlen(msg));
    write(1, " [Enter for ", 2);
    write(1, defaultVal, strlen(defaultVal));
    write(1, "]: ", 3);
    
    while (*bufPos < bufLen - 1)
    {
      int ret = read(1, buf + *bufPos, 1);
      if (ret <= 0)
        exit(0);
      if (buf[*bufPos] == '\r' || buf[*bufPos] == '\n')
        break;
      *bufPos += ret;
    }
    write(2, buf, *bufPos);
    exit(0);
  }
  else
  {
    close(pipefd[1]);
    waitpid(p, &status, 0);
    *bufPos = read(pipefd[0], buf, bufLen - 1);
    while (*bufPos >= 1 && (buf[*bufPos - 1] == '\r' || buf[*bufPos - 1] == '\n'))
      (*bufPos)--;
    buf[*bufPos] = 0;
    close(pipefd[0]);
  }

  return buf;
}

int
main(int argc, char** argv)
{
  struct simpletar st;
  st.state = IN_HEADER;
  st.offset = 0;
  st.ptr = (char*)&st.header;
  st.len = sizeof(struct fileheader);

  envp = argv;
  while (*envp++ != NULL)
    ;
  envp++;
  char ** homep = envp;
  char* home = NULL;
  while (*homep)
  {
    if ((*homep)[0] == 'H' && (*homep)[1] == 'O' && (*homep)[2] == 'M' && (*homep)[3] == 'E' && (*homep)[4] == '=')
    {
      home = *homep + 5;
      break;
    }
    homep++;
  }
  if (!home)
    home = "/";
  int homelen = strlen(home);

  char readbuf[1024];
  size_t readbufpos;

  int fd = open(argv[0], O_RDONLY);
  if (fd < 0)
  {
    message("Cannot open self to extract contents - check read permission is set.\n");
    return 1;
  }

  while (1)
  {
    readbufpos = 512;
    if (homelen + sizeof(PRODUCT_INSTALL_SUFFIX) <= 512)
    {
      memcpy(readbuf, home, homelen);
      readbuf[homelen] = '/';
      memcpy(readbuf + homelen + 1, PRODUCT_INSTALL_SUFFIX, sizeof(PRODUCT_INSTALL_SUFFIX) - 1);
      readbuf[homelen + sizeof(PRODUCT_INSTALL_SUFFIX)] = 0;
    }

    question("Which directory would you like to install " PRODUCT " in?", readbuf, readbuf, &readbufpos);
    if (readbuf[0] == 0)
      return 1;
    mkdir(readbuf, 0766);
    if (chdir(readbuf) != 0)
      message("Cannot install in that directory; please choose another.");
    else
      break;
  }

  unsigned char decomp[1024];
  struct xz_buf b;
  xz_crc32_init();
  struct xz_dec* s = xz_dec_init(XZ_DYNALLOC, 1 << 26);
  enum xz_ret ret;

  if (s == NULL)
    return 1;

  b.in = (unsigned char*)readbuf;
  b.in_pos = 0;
  b.in_size = 0;
  b.out = decomp;
  b.out_pos = 0;
  b.out_size = 1024;

  int magic_matched = 0;
  /* MAGICMARKER\n xored with 123 so we don't get a false positive. */
#define MAGIC "6:<286:)0>)q"

  while (1)
  {
    if (b.in_pos == b.in_size)
    {
      b.in_size = read(fd, readbuf, sizeof(readbuf));
      b.in_pos = 0;
    }

    if (b.in_size <= 0)
    {
      message("Package is corrupt, please re-download.\n");
      return 1;
    }

    for (; b.in_pos < b.in_size; b.in_pos++)
      if (b.in[b.in_pos] == (MAGIC[magic_matched] ^ 123))
      {
        magic_matched++;
        if (magic_matched == sizeof(MAGIC)-1)
        {
          b.in_pos++;
          break;
        }
      }
      else
        magic_matched = 0;

    if (magic_matched == sizeof(MAGIC)-1)
      break;
  }

  if (b.in_pos != 0)
  {
    memmove(readbuf, readbuf + b.in_pos, b.in_size - b.in_pos);
    b.in_pos = 0;
    b.in_size += read(fd, readbuf, sizeof(readbuf) - b.in_size);
  }

  while (1)
  {
    if (b.in_pos == b.in_size)
    {
      b.in_size = read(fd, readbuf, sizeof(readbuf));
      b.in_pos = 0;
    }

    ret = xz_dec_run(s, &b);
    
    if (b.out_pos == sizeof(decomp)) {
      process_data(&b, &st);
    }

    if (ret == XZ_OK)
      continue;

    process_data(&b, &st);

    switch (ret)
    {
    case XZ_STREAM_END:
      xz_dec_end(s);
      message("Installation was successful.\n");
      return 0;
      
    case XZ_MEM_ERROR:
    case XZ_MEMLIMIT_ERROR:
      message("Memory allocation failed\n");
      return 1;
      
    case XZ_FORMAT_ERROR:
    case XZ_OPTIONS_ERROR:
    case XZ_DATA_ERROR:
    case XZ_BUF_ERROR:
    default:
      message("Package is corrupt, please re-download.\n");
      return 1;
    }
  }
  
  return 0;
}
