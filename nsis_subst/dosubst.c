#include <windows.h>
#include <nsis/pluginapi.h>

#define STRINGA "STRINGA78901234567890123456789012345678901234567"
#define STRINGB "STRINGB7890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123"

/* This code is only useful with ASCII-based installers, not UTF16. */

void __declspec(dllexport)
replaceStrings(HWND hwndParent, int string_size, 
               TCHAR *variables, stack_t **stacktop,
               extra_parameters *extra)
{
  char filename[1024], buf[1024], subst1[1024], subst2[1024];
  DWORD numBytesOut;
  int matched, i, done;

  EXDLL_INIT();

  popstring(filename);
  popstring(subst1);
  popstring(subst2);

  HANDLE * hfile = CreateFile(filename, GENERIC_READ | GENERIC_WRITE, 0, NULL, OPEN_ALWAYS, 0, NULL);
  if (hfile == NULL)
    return;

  done = 0;
  matched = 0;
  while (TRUE)
  {
    numBytesOut = 0;
    ReadFile(hfile, buf, sizeof(buf), &numBytesOut, NULL);
    if (numBytesOut == 0)
      break;
    for (i = 0; i < numBytesOut; i++)
    {
      if (STRINGA[matched] == buf[i])
      {
        matched++;
        if (matched == sizeof(STRINGA) - 1)
        {
          SetFilePointer(hfile, -(numBytesOut - i + matched - 1), NULL, FILE_CURRENT);
          WriteFile(hfile, subst1, sizeof(STRINGA) - 1, &numBytesOut, NULL);
          done = 1;
          break;
        }
      }
      else
      {
        if (matched != 0)
          i--;
        matched = 0;
      }
    }
    if (done)
      break;
  }
  SetFilePointer(hfile, 0, NULL, FILE_BEGIN);

  done = 0;
  matched = 0;
  while (TRUE)
  {
    numBytesOut = 0;
    ReadFile(hfile, buf, sizeof(buf), &numBytesOut, NULL);
    if (numBytesOut == 0)
      break;
    for (i = 0; i < numBytesOut; i++)
    {
      if (STRINGB[matched] == buf[i])
      {
        matched++;
        if (matched == sizeof(STRINGB) - 1)
        {
          SetFilePointer(hfile, -(numBytesOut - i + matched - 1), NULL, FILE_CURRENT);
          WriteFile(hfile, subst2, sizeof(STRINGB) - 1, &numBytesOut, NULL);
          done = 1;
          break;
        }
      }
      else
      {
        if (matched != 0)
          i--;
        matched = 0;
      }
    }
  }

  CloseHandle(hfile);
}

BOOL WINAPI
DllMain(HANDLE hInst, ULONG ul_reason_for_call, LPVOID lpReserved)
{
  return TRUE;
}
