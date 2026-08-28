_: {
  perSystem =
    { ps, toolPs, ... }:
    {
      checks = {
        tests = ps.test.check { };
        purs-memo-tests = toolPs.test.check { };
      };
    };
}
