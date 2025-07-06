import React, { useState, useEffect } from 'react';

const EmployeeManager = ({ employees: initialEmployees, pushEvent }) => {
  const [employees, setEmployees] = useState(initialEmployees || []);
  const [showAddForm, setShowAddForm] = useState(false);
  const [formData, setFormData] = useState({
    first_name: '',
    last_name: '',
    email: '',
    phone: '',
    role: 'staff'
  });
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);

  // Update employees when props change
  useEffect(() => {
    setEmployees(initialEmployees || []);
  }, [initialEmployees]);

  const handleInputChange = (e) => {
    const { name, value } = e.target;
    setFormData(prev => ({
      ...prev,
      [name]: value
    }));
  };

  const handleSubmit = async (e) => {
    e.preventDefault();
    setLoading(true);
    setError(null);

    try {
      // Send the create event to LiveView
      pushEvent('create_employee', formData);
      
      // Reset form
      setFormData({
        first_name: '',
        last_name: '',
        email: '',
        phone: '',
        role: 'staff'
      });
      setShowAddForm(false);
    } catch (err) {
      setError('Failed to create employee');
    } finally {
      setLoading(false);
    }
  };

  const handleDelete = (employeeId) => {
    if (window.confirm('Are you sure you want to delete this employee?')) {
      pushEvent('delete_employee', { id: employeeId });
    }
  };

  const getRoleBadgeColor = (role) => {
    switch (role) {
      case 'admin':
        return 'bg-red-100 text-red-800';
      case 'manager':
        return 'bg-blue-100 text-blue-800';
      case 'staff':
        return 'bg-green-100 text-green-800';
      default:
        return 'bg-gray-100 text-gray-800';
    }
  };

  return (
    <div className="space-y-6">
      {/* Header with Add Button */}
      <div className="flex justify-between items-center">
        <h2 className="text-2xl font-bold text-gray-900">Employees</h2>
        <button
          onClick={() => setShowAddForm(!showAddForm)}
          className="bg-blue-600 hover:bg-blue-700 text-white px-4 py-2 rounded-md font-medium transition-colors"
        >
          {showAddForm ? 'Cancel' : 'Add Employee'}
        </button>
      </div>

      {/* Error Display */}
      {error && (
        <div className="bg-red-50 border border-red-200 text-red-700 px-4 py-3 rounded-md">
          {error}
        </div>
      )}

      {/* Add Employee Form */}
      {showAddForm && (
        <div className="bg-gray-50 p-6 rounded-lg border">
          <h3 className="text-lg font-medium text-gray-900 mb-4">Add New Employee</h3>
          <form onSubmit={handleSubmit} className="space-y-4">
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
              <div>
                <label htmlFor="first_name" className="block text-sm font-medium text-gray-700 mb-1">
                  First Name
                </label>
                <input
                  type="text"
                  id="first_name"
                  name="first_name"
                  value={formData.first_name}
                  onChange={handleInputChange}
                  required
                  className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent"
                />
              </div>
              <div>
                <label htmlFor="last_name" className="block text-sm font-medium text-gray-700 mb-1">
                  Last Name
                </label>
                <input
                  type="text"
                  id="last_name"
                  name="last_name"
                  value={formData.last_name}
                  onChange={handleInputChange}
                  required
                  className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent"
                />
              </div>
            </div>
            
            <div>
              <label htmlFor="email" className="block text-sm font-medium text-gray-700 mb-1">
                Email
              </label>
              <input
                type="email"
                id="email"
                name="email"
                value={formData.email}
                onChange={handleInputChange}
                required
                className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent"
              />
            </div>
            
            <div>
              <label htmlFor="phone" className="block text-sm font-medium text-gray-700 mb-1">
                Phone (Optional)
              </label>
              <input
                type="tel"
                id="phone"
                name="phone"
                value={formData.phone}
                onChange={handleInputChange}
                className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent"
              />
            </div>
            
            <div>
              <label htmlFor="role" className="block text-sm font-medium text-gray-700 mb-1">
                Role
              </label>
              <select
                id="role"
                name="role"
                value={formData.role}
                onChange={handleInputChange}
                className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent"
              >
                <option value="staff">Staff</option>
                <option value="manager">Manager</option>
                <option value="admin">Admin</option>
              </select>
            </div>
            
            <div className="flex justify-end space-x-3">
              <button
                type="button"
                onClick={() => setShowAddForm(false)}
                className="px-4 py-2 text-gray-700 bg-gray-200 hover:bg-gray-300 rounded-md font-medium transition-colors"
              >
                Cancel
              </button>
              <button
                type="submit"
                disabled={loading}
                className="px-4 py-2 bg-blue-600 hover:bg-blue-700 text-white rounded-md font-medium transition-colors disabled:opacity-50"
              >
                {loading ? 'Creating...' : 'Create Employee'}
              </button>
            </div>
          </form>
        </div>
      )}

      {/* Employee List */}
      <div className="bg-white shadow overflow-hidden sm:rounded-md">
        {employees.length === 0 ? (
          <div className="text-center py-12">
            <p className="text-gray-500 text-lg">No employees found</p>
            <p className="text-gray-400 text-sm mt-2">Add your first employee to get started</p>
          </div>
        ) : (
          <ul className="divide-y divide-gray-200">
            {employees.map((employee) => (
              <li key={employee.id} className="px-6 py-4">
                <div className="flex items-center justify-between">
                  <div className="flex-1">
                    <div className="flex items-center space-x-3">
                      <h3 className="text-lg font-medium text-gray-900">
                        {employee.full_name || `${employee.first_name} ${employee.last_name}`}
                      </h3>
                      <span className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${getRoleBadgeColor(employee.role)}`}>
                        {employee.role}
                      </span>
                    </div>
                    <div className="mt-1 text-sm text-gray-600 space-y-1">
                      <p>📧 {employee.email}</p>
                      {employee.phone && <p>📞 {employee.phone}</p>}
                    </div>
                  </div>
                  <div className="flex items-center space-x-2">
                    <button
                      onClick={() => handleDelete(employee.id)}
                      className="text-red-600 hover:text-red-800 font-medium text-sm transition-colors"
                    >
                      Delete
                    </button>
                  </div>
                </div>
              </li>
            ))}
          </ul>
        )}
      </div>
    </div>
  );
};

export default EmployeeManager;
