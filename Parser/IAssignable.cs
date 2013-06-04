using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Osprey.Nodes
{
	public interface IAssignable
	{
		void CompileSimpleAssignment(Compiler compiler, MethodBuilder method, Expression value, bool useValue);

		void CompileCompoundAssignment(Compiler compiler, MethodBuilder method, Expression value, BinaryOperator op);
	}
}