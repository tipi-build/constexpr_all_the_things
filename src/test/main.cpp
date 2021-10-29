void object_value_tests();
namespace test_strings_as_types { void ensure_addressable_types(); }

int main(void)
{
  object_value_tests();
  test_strings_as_types::ensure_addressable_types();
}
