let () =
  Alcotest.run "op"
    [
      Test_util.suite;
      Test_prompt.suite;
      Test_config.suite;
      Test_transport.suite;
      Test_read.suite;
      Test_write.suite;
      Test_edit.suite;
      Test_nu.suite;
      Test_nu_sandbox.suite;
      Test_context.suite;
      Test_agent.suite;
      Test_gate.suite;
      Test_gate_nu.suite;
      Test_session.suite;
      Test_session_read.suite;
      Test_openai_adapter.suite;
      Test_openai_responses_adapter.suite;
      Test_anthropic_adapter.suite;
      Test_google_generative_ai_adapter.suite;
      Test_client.suite;
      Test_tool.suite;
      Test_tools.suite;
      Test_image.suite;
    ]
