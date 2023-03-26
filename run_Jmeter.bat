FOR /l %%k IN (1, 1, 4) DO (
  md %%k
  FOR /l %%i IN (1, 1, 15) DO (
    D:\Programy\apache-jmeter-5.5\apache-jmeter-5.5\bin\jmeter.bat -n -t Pi2_parametrised.jmx -l log.jtl -Jusers=%%i -Jiteration=%%k
    @TIMEOUT /t 3 >nul
  )
)


