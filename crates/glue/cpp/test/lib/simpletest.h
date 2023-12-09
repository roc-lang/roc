#pragma once

//---------------------------------------------------------------------------------
// Config
//---------------------------------------------------------------------------------
#if !defined(MESSAGE_SPACE)
#define MESSAGE_SPACE 10 * 1024 // default 10k of message space is reserved per test
#endif
#if !defined(STRING_LENGTH)
#define STRING_LENGTH 64 // size of temp strings for converting types
#endif
#if !defined(STRING_EQ_PRINT_LENGTH)
#define STRING_EQ_PRINT_LENGTH 80 // max line length to show when comparing two strings
#endif
#if !defined(BASE_FIXTURE)
#define BASE_FIXTURE TestFixture // use TestFixture as the test base class by default
#endif
#if !defined(ERROR_ACTION)
#define ERROR_ACTION // Defined any code to run on error. You can use this to debug break or do anything really
#endif

//---------------------------------------------------------------------------------
// Link list of errors build into MESSAGE_SPACE
//---------------------------------------------------------------------------------
struct TestError
{
	TestError* next;
	char message[1];
};

//---------------------------------------------------------------------------------
// simple converter of basic types to text
// TODO: Use a global template function for conversion so users can override this
//---------------------------------------------------------------------------------
struct TempString
{
	TempString() : myTextPointer(myTextBuffer) { myTextBuffer[0] = 0; }
	TempString(const TempString& other);
	TempString(char const* string) : myTextPointer(string) {}

	char const* operator*() const { return myTextPointer; }

	char const* myTextPointer;
	char myTextBuffer[STRING_LENGTH];
};

TempString TypeToString(int value);
TempString TypeToString(unsigned int value);
TempString TypeToString(long value);
TempString TypeToString(unsigned long value);
TempString TypeToString(long long value);
TempString TypeToString(unsigned long long value);
TempString TypeToString(float value);
TempString TypeToString(double value);
TempString TypeToString(bool value);
TempString TypeToString(char const* value);
TempString TypeToString(void const* value);
TempString TypeToString(void const* value, char const* extra);

inline TempString TypeToString(char value) { return TypeToString((int)value); }
inline TempString TypeToString(unsigned char value) { return TypeToString((unsigned int)value); }
inline TempString TypeToString(short value) { return TypeToString((int)value); }
inline TempString TypeToString(unsigned short value) { return TypeToString((unsigned int)value); }
inline TempString TypeToString(char* value) { return TypeToString((char const*)value); }
inline TempString TypeToString(void* value) { return TypeToString((void const*)value); }

// if nothing specified then print some memory
template<typename T>
TempString TypeToString(T const&) { return TempString(); }

template<typename T>
TempString TypeToString(T const* pointer)
{
	return pointer == nullptr ?
		TypeToString((void const*)pointer) :
		TypeToString((void const*)pointer, *TypeToString(*pointer));
}

template<typename T>
TempString TypeToString(T* pointer) { return TypeToString((T const*)pointer); }

inline TempString TypeToStringFallback(TempString string, char const* fallback) { return (*string)[0] ?  string : TempString(fallback); }

//---------------------------------------------------------------------------------
// Test fixture is the core of SimpleTest. It provides fixture behavior, access
// to registered tests and stores the results of a test run
// Everything is local here so tests can be multithreaded without any extra work
//---------------------------------------------------------------------------------
class TestFixture
{
public:
	TestFixture();
	virtual ~TestFixture() {};

	virtual bool ExecuteTest();

	virtual char const* TestName() const = 0;
	virtual char const* TestGroup() const = 0;

	// Reporting used during testing process
	void AddTest() { ++myNumTestsChecked; }
	void AddError() { ++myNumErrors; }
	void LogMessage(char const* string, ...);

	// Custom test for strings to print out where the comparison failed
	bool TestStrings(char const* left, char const* right, char const* prefix, char const* condition);

	// Stats from execution
	int NumTests() const { return myNumTestsChecked; }
	int NumErrors() const { return myNumErrors; }

	// Access to any errrors generated
	TestError const* GetFirstError() const { return (TestError*)myMessageSpace; }
	TestError const* GetLastError() const { return myNextError; }

	// Access to registered tests
	static TestFixture* GetFirstTest() { return ourFirstTest; }
	static TestFixture* GetCurrentTest() { return ourCurrentTest; }
	TestFixture* GetNextTest() const { return myNextTest; }

	enum OutputMode
	{
		Silent,
		Normal,
		Verbose
	};

	enum PrintMethod
	{
		PrintDefault,
		PrintHexadecimal,
	};

	PrintMethod GetPrintMethod() const { return myPrintMethod; }
	void SetPrintMethod(PrintMethod aPrintMethod) { myPrintMethod = aPrintMethod; }

	// Default execution implementation
	static void (*Print)(char const* string);
	static void Printf(char const* string, ...);

	static bool ExecuteAllTests(char const* groupFilter = nullptr, char const* nameFilter = nullptr, OutputMode output = Normal);
	static bool ExecuteAllTests(OutputMode output) { return ExecuteAllTests(nullptr, nullptr, output); }

	static bool ExecuteTestGroup(char const* groupFilter, OutputMode output = Normal) { return ExecuteAllTests(groupFilter, nullptr, output); }

protected:
	virtual void RunTest() = 0;
	virtual void Setup() {}
	virtual void TearDown() {}
	
	// Test registration
	static TestFixture const* LinkTest(TestFixture* test);
	static TestFixture* ourFirstTest;
	static TestFixture* ourLastTest;

	TestFixture* myNextTest;
	TestError* myNextError;

	int myNumTestsChecked;
	int myNumErrors;

	PrintMethod myPrintMethod;

	char myMessageSpace[MESSAGE_SPACE];

	// allow access to current test outside of main code block
	static thread_local TestFixture* ourCurrentTest;
};

//---------------------------------------------------------------------------------
// Test definition macros
//---------------------------------------------------------------------------------
#define DEFINE_TEST_FULL(name, group, fixture) \
struct TOK(group, name) final : public fixture { \
	char const* TestName() const override { return #name; } \
	char const* TestGroup() const override { return #group; } \
	void RunTest() override; \
} TOK(TOK(group, name), Instance); \
void TOK(group, name)::RunTest()

#define DEFINE_TEST(name) DEFINE_TEST_FULL(name, Global, BASE_FIXTURE)
#define DEFINE_TEST_G(name, group) DEFINE_TEST_FULL(name, group, BASE_FIXTURE)
#define DEFINE_TEST_F(name, fixture) DEFINE_TEST_FULL(name, Global, fixture)
#define DEFINE_TEST_GF(name, group, fixture) DEFINE_TEST_FULL(name, group, fixture)

//---------------------------------------------------------------------------------
// Utils
//---------------------------------------------------------------------------------
template <typename T>
T TestDifference(T const& a, T const& b) { return a > b ? a - b : b - a; }

// why are these still needed?
#define STR2(x) #x
#define STR(x) STR2(x)

#define TOK2(a, b) a ## b
#define TOK(a, b) TOK2(a, b)

//---------------------------------------------------------------------------------
// Error reporting and setup, don't call directly
//---------------------------------------------------------------------------------
#define TEST_TYPE_TO_STRING(var, arg) *TypeToStringFallback(TypeToString(var), STR(arg))
#define TEST_ERROR_PREFIX_ __FILE__ "(" STR(__LINE__) "): Condition [%s] Failed. "
#define TEST_ERROR_(message, ...) do { TestFixture* __fx = TestFixture::GetCurrentTest(); __fx->AddError(); __fx->LogMessage(TEST_ERROR_PREFIX_ message, ##__VA_ARGS__); ERROR_ACTION; } while(0)
#define TEST_BEGIN_(a) do { auto const& test_value_ = a
#define TEST_CHECK_(cond, condtext, message, ...) do { TestFixture::GetCurrentTest()->AddTest(); if (!(cond)) TEST_ERROR_(message, condtext, ##__VA_ARGS__); } while(0)
#define TEST_END_ } while(0)

//---------------------------------------------------------------------------------
// Tests
//
// Note: Value caching is only enabled on left hand side. This splits the difference
// between preventing side effects (i.e. x++ double incrementing) and allowing the
// compiler to infer values (i.e. TEST_EQ(unsigned(1), 1) will try to cache 1 as an int then omit a compile warning).
// This means that the right hand side will get evaluated multiple times, so please avoid
// expressions like: TEST_EQ(a++, b++) as they won't work. Tests should always be written
// as following:
// TEST_EQ(expression, constant)
//---------------------------------------------------------------------------------
#define TEST_OPERATOR(a, b, op1, op2) TEST_BEGIN_(a); TEST_CHECK_((test_value_) op1 (b), STR(a) " " STR(op1) " " STR(b), "'%s' " STR(op2) " '%s'", TEST_TYPE_TO_STRING(test_value_, a), TEST_TYPE_TO_STRING(b, b)); TEST_END_

#define TEST(cond) TEST_EQ(cond, true)
#define TEST_FAIL(cond) TEST_EQ(cond, false)

#define TEST_EQ(a, b) TEST_OPERATOR(a, b, ==, !=)
#define TEST_NEQ(a, b) TEST_OPERATOR(a, b, !=, ==)
#define TEST_GREATER(a, b) TEST_OPERATOR(a, b, >, <=)
#define TEST_GREATER_EQUAL(a, b) TEST_OPERATOR(a, b, >=, <)
#define TEST_LESS(a, b) TEST_OPERATOR(a, b, <, >=)
#define TEST_LESS_EQUAL(a, b) TEST_OPERATOR(a, b, <=, >)

#define TEST_STR_EQ(a, b) do { if(!TestFixture::GetCurrentTest()->TestStrings(a, b, TEST_ERROR_PREFIX_ "\n%s\n%s\n%s", STR(a) " == " STR(b))) { ERROR_ACTION; } } while(0)
#define TEST_CLOSE(a, b, eps) TEST_BEGIN_(TestDifference(a, b)); TEST_CHECK_(test_value_ <= eps, STR(a) " Close to " STR(b), "Difference of %s is greater than expected amount of " STR(eps) " when comparing %s and %s", TEST_TYPE_TO_STRING(test_value_, TestDifference(a, b)), TEST_TYPE_TO_STRING(a, a), TEST_TYPE_TO_STRING(b, b)); TEST_END_
#define TEST_DIFFERS(a, b, eps) TEST_BEGIN_(TestDifference(a, b)); TEST_CHECK_(test_value_ >= eps, STR(a) " Differs from " STR(b), "Difference of %s is less than expected amount of " STR(eps) " when comparing %s and %s", TEST_TYPE_TO_STRING(test_value_, TestDifference(a, b)), TEST_TYPE_TO_STRING(a, a), TEST_TYPE_TO_STRING(b, b)); TEST_END_
#define TEST_MESSAGE(cond, message, ...) TEST_BEGIN_(cond); TEST_CHECK_(test_value_, STR(cond), message, ##__VA_ARGS__); TEST_END_
