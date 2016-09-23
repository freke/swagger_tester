Swagger Tester
=====

An OTP library making it easy to test Swagger REST API:s. Main focus is to simplify testing cowboy_swagger appications in combination with common test.

See test for example.

Use
-----
in rebar.config add

    {profiles,
      [
        {test,
          [
            {deps,
              [
                {swagger_tester, {git, "git://github.com/freke/swagger_tester.git", {branch, "master"}}}
              ]
            }
          ]
        }
      ]
    }
