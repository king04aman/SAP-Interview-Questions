# SAP ABAP Interview Questions

*A comprehensive set of SAP ABAP Interview Questions designed to help both novice and experienced professionals excel in SAP ABAP job interviews. Covering core ABAP programming concepts, Data Dictionary, ALV reporting, enhancements (Exits, BAdIs), OOP best practices, performance tuning, and advanced integrations (RFC, IDoc, BAPI, SAP HANA), this list ensures you’re fully prepared to demonstrate your expertise in SAP’s leading development environment. Perfect for anyone aiming to sharpen their ABAP skills and boost their career in the evolving SAP landscape.*

---
## Table Of Content

### SAP ABAP Interview Question

1. What does ABAP stand for, and which core SAP platform was it originally designed for?
2. Why is ABAP often considered a “4GL” (fourth-generation language)?
3. In which SAP transaction can you create or edit an ABAP program from scratch?
4. How do you execute a simple “Hello World” report program in SAP?
5. What is a “report” in ABAP, and how does it differ from a “dialog” program?
6. In ABAP, what are event blocks like `START-OF-SELECTION` and `END-OF-SELECTION` used for?
7. What is the significance of the `WRITE` statement in a classical ABAP report?
8. Can you briefly explain the difference between the “Classical ABAP Editor” and the “ABAP in Eclipse” environment?
9. How do you create a local text symbol in ABAP, and why is it useful?
10. What is the difference between `SY-UCOMM` and `SY-BCODE` in interactive reports?

11. Where do you typically maintain program attributes (title, type, etc.) in SAP ABAP?
12. What is an internal table in ABAP, and why is it commonly used?
13. How do you define a simple internal table using the `DATA` statement in modern ABAP syntax (ABAP 7.4+)?
14. Give an example of how you might append a new row to an internal table in ABAP.
15. Why must you often sort an internal table before performing a binary search or read operation?
16. What are the differences between STANDARD, SORTED, and HASHED internal tables?
17. How do you define a work area vs. a field symbol for internal table processing?
18. What is the purpose of the `LOOP AT itab` statement, and how do you end that loop?
19. In modern ABAP, how does `FOR ... IN` expressions differ from classical `LOOP` syntax?
20. Why might you use `READ TABLE ... TRANSPORTING NO FIELDS` in some performance-critical scenarios?

21. What is a subroutine (`PERFORM` / `FORM`) in ABAP, and why might it be replaced by function modules or methods?
22. How do you declare and call a function module using the older function group approach?
23. Explain the difference between pass-by-value and pass-by-reference parameters in ABAP.
24. Why does ABAP commonly rely on modularization (subroutines, function modules) for maintainability?
25. What is the significance of the `SY-SUBRC` system field after many ABAP operations?
26. Can you provide an example of using `IF sy-subrc = 0` to check the success of a statement?
27. Which transaction code do you use to view or debug short dumps (runtime errors) in SAP?
28. How do you typically debug an ABAP program step by step? Which transaction(s) or tool do you use?
29. Why might you use breakpoints (`BREAK-POINT`) or watchpoints in ABAP debugging?
30. What is the difference between a static breakpoint and a dynamic breakpoint?

31. How do you manage or version ABAP code changes across development, test, and production systems?
32. Which SAP Workbench Organizer transaction handles change requests and transports?
33. Why is it recommended to group related ABAP objects in a single transport request?
34. How might you revert or roll back a transport that has already been imported into QA but not into production?
35. What is the difference between a Workbench request and a Customizing request in SAP?
36. In your own words, what is the Data Dictionary (DDIC) in ABAP?
37. How do you create a transparent table in the DDIC with fields, data elements, and domains?
38. What is a domain vs. a data element? How do they work together in the DDIC?
39. Explain the difference between a transparent table and a pooled/cluster table.
40. Why are foreign key relationships important in the Data Dictionary?

41. How do you define search helps (elementary or collective) for user-friendly value lookups in SAP screens?
42. When creating a table, how do you specify primary keys and what is the role of technical settings (like buffering)?
43. Why might you choose partial or full buffering for a table, and what are the trade-offs?
44. Can you describe how you’d add a new field to an existing table in production without losing data?
45. What is the purpose of the “Delivery Class” (e.g., A, C, L) in a table’s technical settings?
46. How do you create a check table to enforce referential integrity for a given field?
47. What is the advantage of using domains with fixed values for enumerations?
48. Explain how you’d create a table maintenance generator in transaction SE11 or SE54 for direct table data edits.
49. Why do some tables require an append structure approach for SAP standard tables?
50. What does “activation” mean in the context of Data Dictionary objects?

51. How do you define a classical ABAP report to output a simple list with headings and line items?
52. What is the significance of “List Processing” in classical reporting, and how do you handle secondary lists?
53. Explain how the `AT USER-COMMAND` event is used for interactive reports.
54. In older function module-based ALV (REUSE_ALV_LIST_DISPLAY), what are the key parameters you must pass for field catalogs?
55. Compare the classical ALV function modules approach with the OOP-based ALV Grid approach (`CL_GUI_ALV_GRID`).
56. How do you create an interactive ALV where double-clicking a row triggers a detailed view in a second screen?
57. What is the difference between ALV List and ALV Grid? Why might you choose one over the other?
58. In a scenario: you need to highlight rows in red if a quantity is negative. Which ALV technique do you use?
59. How might you integrate selection screens with ALV output in a single ABAP report program?
60. If you need a custom toolbar button in ALV, how do you add it and handle its user command?

61. What is SAP Script, and in which scenarios was it historically used?
62. Differentiate between SAP Script, Smart Forms, and Adobe Forms as form printing technologies.
63. If a user wants a PDF-based official invoice form, which SAP printing approach might you recommend and why?
64. How do you pass data from an ABAP program to a Smart Form for dynamic text or fields?
65. Why might you prefer a “driver program + form” design pattern for printing application documents?
66. In Smart Forms, how do you handle conditions or logic for printing certain sections (e.g., only if item quantity is above 0)?
67. Can you briefly explain the concept of windows vs. pages in Smart Forms?
68. If you want to migrate an SAP Script layout to Smart Forms, is there any tool or direct migration path?
69. Describe a scenario in which you’d implement custom logic inside the form using form routines vs. separate function modules.
70. How do you debug layout logic or text routines inside a Smart Form?

71. In ABAP, what is a “Customer Exit” and how does it differ from a “User Exit”?
72. What is a BAdI (Business Add-In), and how do you typically implement it in code?
73. How do implicit and explicit enhancements differ from classical user exits or BAdIs?
74. If you want to add a new check in standard SAP code without modifying it, which enhancement framework options exist?
75. Provide an example of using transaction SMOD/CMOD for user exits in SAP standard modules.
76. What is the Enhancement Spot concept in newer ABAP releases, and how do you define or implement them?
77. When might you use a “Multiple Use BAdI” instead of a “Single Use BAdI”?
78. How do you handle BAdI filter values, and why might that be relevant for scenario-specific logic?
79. Describe how you might search for available user exits or BAdIs in SAP standard transactions.
80. In your own words, how do enhancements help keep standard code “upgrade-safe”?

81. What is OOP in ABAP, and how does it differ from older “procedural” ABAP?
82. Explain the significance of classes, methods, and interfaces in ABAP Objects.
83. Give a small code snippet showing how to define a global class in SE24 with a public method.
84. Why might you define an interface if you can simply define an abstract class?
85. In an OO ALV scenario: how does the `CL_SALV_TABLE` approach simplify list creation compared to older function modules?
86. How do you create attributes or methods that are only visible within the class (private) vs. visible outside (public)?
87. Explain constructor methods in ABAP Objects. How do they differ from class constructors vs. instance constructors?
88. If you want to override a method from a superclass, which syntax do you use?
89. What is a “final” class or method in ABAP, and why might you declare it as final?
90. How can you handle event handling in ABAP Objects (publish-subscribe pattern within classes)?

91. Define RFC (Remote Function Call) in ABAP. How do you mark a function module as remote-enabled?
92. Provide a scenario where you’d use tRFC or qRFC instead of sRFC. Why?
93. If you want to call an ABAP function from an external system (e.g., Java or .NET), how do you expose it?
94. How do you handle connection errors or timeouts when calling an RFC from outside SAP?
95. What is a BAPI, and how does it differ from a general RFC-enabled function module?
96. In which SAP transaction code do you typically search for existing BAPIs?
97. If you want to create a new sales order from an external system, which standard BAPI might you consider?
98. How do you test an RFC or BAPI quickly from within SAP (without writing a new ABAP program)?
99. What role does the IDoc interface play if your external integration requires standard EDI-like data exchange?
100. Compare the usage of IDoc-based integration vs. RFC-based integration in a typical SAP scenario.


---

### Happy Learning 🎉🎉🎉